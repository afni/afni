#include "SUMA_suma.h"

SUMA_SurfaceViewer *SUMAg_cSV = NULL; /*!< Global pointer to current Surface Viewer structure*/
SUMA_SurfaceViewer *SUMAg_SVv = NULL; /*!< Global pointer to the vector containing the various Surface Viewer Structures 
                                    SUMAg_SVv contains SUMA_MAX_SURF_VIEWERS structures */
int SUMAg_N_SVv = 0; /*!< Number of SVs realized by X */
SUMA_DO *SUMAg_DOv = NULL;   /*!< Global pointer to Displayable Object structure vector*/
int SUMAg_N_DOv = 0; /*!< Number of DOs stored in DOv */
SUMA_CommonFields *SUMAg_CF = NULL; /*!< Global pointer to structure containing info common to all viewers */


void usage_SUMA_SurfSmooth (SUMA_GENERIC_ARGV_PARSE *ps)
   {
      static char FuncName[]={"usage_SUMA_SurfSmooth"};
      char * s = NULL, *st = NULL, *sm = NULL, *sio=NULL;
      s = SUMA_help_basics();
      sio  = SUMA_help_IO_Args(ps);
      st = SUMA_help_talk();
      sm = SUMA_help_mask();
      printf (
"\nUsage:  SurfSmooth <-SURF_1> <-met method> \n"
"\n"
"   Some methods require additional options detailed below.\n"
"   I recommend using the -talk_suma option to watch the \n"
"   progression of the smoothing in real-time in suma.\n"
"\n"
"   Method specific options:\n"
"      HEAT_07: <-input inData.1D> <-target_fwhm F>   \n"
"            This method is used to filter data\n"
"            on the surface. It is a significant\n"
"            improvement on HEAT_05.\n"
"      HEAT_05: <-input inData.1D> <-fwhm F>  \n"
"            Formerly known as HEAT, this method is used \n"
"            to filter data on the surface. \n"
"            Parameter choice is tricky however as one\n"
"            needs to take into account mesh dimensions,\n"
"            desired FWHM, and the data's starting FWHM in \n"
"            order to make an appropriate selection.\n"
"            Consider using HEAT_07 if applicable.\n"
"            Note that this version will select the number\n"
"            of iterations to avoid precision errors.\n"
/*            "      LB_FEM: <-input inData.1D> <-fwhm f>\n"
"              This method is used to filter data\n"
"              on the surface.\n" */
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
"      [-add_index] [-ni_text|-ni_binary] [-talk_suma] [-MASK] \n\n"
"\n"
"   Detailed usage:\n"
"     (-SURF_1):  An option for specifying the surface to smooth or\n"
"                 the domain over which DSET is defined.\n"
"                 (For option's syntax, see 'Specifying input surfaces'\n"
"                 section below).\n"
"     (-MASK)  :  An option to specify a node mask so that only\n"
"                 nodes in the mask are used in the smoothing.\n"
"                 See section 'SUMA mask options' for details on\n"
"                 the masking options.\n"
"      -met method: name of smoothing method to use. Choose from:\n"
"                 HEAT_07: A significant improvement on HEAT_05.\n" 
"                         This method is used for filtering \n"
"                         data on the surface and not for smoothing \n"
"                         the surface's geometry per se. \n"
"                         This method makes more appropriate parameter\n"
"                         choices that take into account:\n"
"                         - Numerical precision issues\n"
"                         - Mesh resolution\n"
"                         - Starting and Target FWHM\n"
"                 HEAT_05: The newer method by Chung et al. [Ref. 3&4 below]\n"
"                         Consider using HEAT_07 if applicable.\n" 
/*    "                 LB_FEM: (Olde, better use HEAT)\n"
"                         The method by Chung et al. 03.\n"
"                         This method is used for filtering \n"
"                         data on the surface and not for smoothing the\n"
"                         surface's geometry per se. See References below.\n" */
"                 LM: The smoothing method proposed by G. Taubin 2000\n"
"                     This method is used for smoothing\n"
"                     a surface's geometry. See References below.\n"
"                 NN_geom: A simple nearest neighbor coordinate smoothing.\n"
"                          This interpolation method causes surface shrinkage\n"
"                          that might need to be corrected with the -match_*\n"
"                          options below. \n" 
"\n"
/*            "   Options for LB_FEM:\n"
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
"               of the Euclidean distances. See Ref #1 for more details \n"
"               on this parameter.\n"   */ 
"\n"
"   Options for HEAT_07 (see @SurfSmooth.HEAT_07.examples for examples):\n"
"      -input inData : file containing data (in 1D or NIML format)\n"
"                        Each column in inData is processed separately.\n"
"                        The number of rows must equal the number of\n"
"                        nodes in the surface. You can select certain\n"
"                        columns using the [] notation adopted by AFNI's\n"
"                  Note: The program will infer the format of the input\n"
"                        file from the extension of inData. \n" 
"                        programs.\n"
"      -fwhm F: Blur by a Gaussian filter that has a Full Width at Half \n"
"               Maximum in surface coordinate units (usuallly mm) of F.\n"
"               For Gaussian filters, FWHM, SIGMA (STD-DEV) and RMS\n"
"               FWHM = 2.354820 * SIGMA = 1.359556 * RMS\n"
"               The program first estimates the initial dataset's smoothness\n"
"               and determines the final FWHM (FF) that would result from \n"
"               the added blurring by the filter of width F.\n"  
"               The progression of FWHM is estimated with each iteration, \n"
"               and the program stops when the dataset's smoothness reaches\n"
"               FF.\n"
"   or \n"
"      -target_fwhm TF: Blur so that the final FWHM of the data is TF mm\n"
"                       This option avoids blurring already smooth data.\n"
"                       FWHM estimates are obtained from all the data\n"
"                       to be processed.\n"
"      -blurmaster BLURMASTER: Blur so that the final FWHM of dataset\n"
"                       BLURMASTER is TF mm, then use the same blurring\n"
"                       parameters on inData. In most cases, \n"
"                       you ought to use the -blurmaster option in \n"
"                       conjunction with options -fwhm and target_fwhm.\n"
"                       BLURMASTER is preferably the residual timeseries \n"
"                       (errts)  from 3dDeconvolve. \n"
"                       If using the residual is impractical, you can \n"
"                       use the epi time series with detrending option below.\n"
"                       The two approaches give similar results for block \n"
"                       design data  but we have not checked for randomised\n"
"                       event related designs.\n"
"                       After detrending (see option -detrend_master), a \n"
"                       subset of sub-bricks will be selected for estimating \n"
"                       the smoothness.\n"
"                       Using all the sub-bricks would slow the program down.\n"
"                       The selection is similar to what is done in \n"
"                       3dBlurToFWHM.\n"
"                       At most 32 sub-bricks are used and they are selected \n"
"                       to be scattered throughout the timeseries. You can\n"
"                       use -bmall to force the use of all sub-bricks.\n"
"                 N.B.: Blurmaster must be a time series with a continuous\n"
"                       time axis. No catenated time series should be used\n"
"                       here.\n" 
"      -detrend_master [q]: Detrend blurmaster with 2*q+3 basis functions \n"
"                           with q > 0.\n"
"                         default is -1 where q = NT/30.\n"
"                         This option should be used when BLURMASTER is an\n"
"                         epi time series.\n"
"                         There is no need for detrending when BLURMASTER \n"
"                         is the residual\n"
"                         from a linear regression analysis.\n"
"      -no_detrend_master: Do not detrend the master. That would be used \n"
"                          if you are using residuals for master.\n"
"      -detpoly_master p: Detrend blurmaster with polynomials of order p.\n"
"      -detprefix_master d: Save the detrended blurmaster into a dataset \n"
"                           with prefix 'd'.\n"
"      -bmall: Use all sub-bricks in master for FWHM estimation.\n"
"      -detrend_in [q]: Detrend input before blurring it, then retrend \n"
"                       it afterwards. Default is no detrending.\n"
"                       Detrending mode is similar to detrend_master.\n"
"      -detpoly_in p: Detrend input before blurring then retrend.\n"
"                     Detrending mode is similar to detpoly_master.\n"
"      -detprefix_in d Save the detrended input into a dataset with\n"
"                      prefix 'd'.\n"
"\n"
"   and optionally, one of the following two parameters:\n"
"      -Niter N: Number of iterations (default is -1).\n"
"                You can now set this parameter to -1 and have \n"
"                the program suggest a value based on the surface's\n"
"                mesh density (average distance between nodes), \n"
"                the desired and starting FWHM. \n"
"                Too large or too small a number of iterations can affect \n"
"                smoothing results. \n"
"      -sigma  S: Bandwidth of smoothing kernel (for a single iteration).\n"
"                 S should be small (< 1) but not too small.\n"
"                 If the program is taking forever to run, with final\n"
"                 numbers of iteration in the upper hundreds, you can\n"
"                 increase the value of -sigma somewhat.\n"
"      -c_mask or -b_mask or -n_mask (see below for details):\n"
"                 Restrict smoothing to nodes in mask.\n"
"                 You should not include nodes with no data in \n"
"                 the smoothing. Note that the mask is also applied \n"
"                 to -blurmaster dataset and all estimations of FWHM.\n"
"                 For example:\n"
"                    If masked nodes have 0 for value in the input \n"
"                    dataset's first (0th) sub-brick, use: \n"
"                    -cmask '-a inData[0] -expr bool(a)'\n"
"   Notes:\n"
"   1- For those of you who know what they are doing, you can also skip \n"
"   specifying fwhm options and specify Niter and sigma directly.\n"
"\n"
"   Options for HEAT_05  (Consider HEAT_07 method):\n"
"      -input inData : file containing data (in 1D or NIML format)\n"
"                        Each column in inData is processed separately.\n"
"                        The number of rows must equal the number of\n"
"                        nodes in the surface. You can select certain\n"
"                        columns using the [] notation adopted by AFNI's\n"
"                  Note: The program will infer the format of the input\n"
"                        file from the extension of inData. \n" 
"                        programs.\n"
"      -fwhm F: Effective Full Width at Half Maximum in surface \n"
"               coordinate units (usuallly mm) \n"
"               of an equivalent Gaussian filter had the surface been flat.\n"
"               With curved surfaces, the equation used to estimate FWHM is \n"
"               an approximation. For Gaussian filters, FWHM, SIGMA \n"
"               (STD-DEV) and RMS are related by:\n"
"               FWHM = 2.354820 * SIGMA = 1.359556 * RMS\n"
"               Blurring on the surface depends on the geodesic instead \n"
"               of the Euclidean distances. \n"
"               Unlike with HEAT_07, no attempt is made here at direct\n"
"               estimation of smoothness.\n"
"\n"
"      Optionally, you can add one of the following two parameters:\n"
"                     (See Refs #3&4 for more details)\n"
"      -Niter N: Number of iterations (default is -1).\n"
"                You can now set this parameter to -1 and have \n"
"                the program suggest a value based on the -fwhm value.\n"
"                Too large or too small a number of iterations can affect \n"
"                smoothing results. Acceptable values depend on \n"
"                the average distance between nodes on the mesh and\n"
"                the desired fwhm. \n"
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
"          NOTE 1: For HEAT method, you can set Niter to -1, in conjunction\n"
"                  with -fwhm FWHM option, and the program\n"
"                  will pick an acceptable number for you.\n"
"          NOTE 2: For LB_FEM method, the number of iterations controls the\n"
"                iteration steps (dt in Ref #1).\n"
"                dt = fwhm*fwhm / (16*Niter*log(2));\n"
"                dt must satisfy conditions that depend on the internodal\n"
"                distance and the spatial derivatives of the signals being \n"
"                filtered on the surface.\n"
"                As a rule of thumb, if increasing Niter does not alter\n"
"                the results then your choice is fine (smoothing has\n"
"                converged).\n"
"                For an example of the artifact caused by small Niter see:\n"
"          http://afni.nimh.nih.gov/sscc/staff/ziad/SUMA/SuSmArt/DSart.html\n"
"                To avoid this problem altogether, it is better that you use \n"
"                the newer method HEAT which does not suffer from this\n"
"                problem.\n"
"      -output OUT: Name of output file. \n"
"                   The default is inData_sm with LB_FEM and HEAT method\n"
"                   and NodeList_sm with LM method.\n" 
"             NOTE: For data smoothing methods like HEAT, If a format\n"
"                   extension, such as .1D.dset or .niml.dset is present \n"
"                   in OUT, then the output will be written in that format.\n"
"                   Otherwise, the format is the same as the input's\n"
"      -overwrite : A flag to allow overwriting OUT\n"
"      -add_index : Output the node index in the first column.\n"
"                   This is not done by default.\n"
"      -dbg_n node : output debug information for node 'node'.\n"
"      -use_neighbors_outside_mask: When using -c_mask or -b_mask or -n_mask\n"
"                                   options, allow value from a node nj \n"
"                                   neighboring node n to contribute to the \n"
"                                   value at n even if nj is not in the mask.\n"
"                                   The default is to ignore all nodes not in\n" "                                   the mask.\n"
"\n"
"%s"
"\n"
"%s"
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
" \n"     
"      For HEAT_07 method, see multiple examples with data in script\n"
"                  @SurfSmooth.HEAT_07.examples\n"
"\n"       
"      SurfSmooth  -spec quick.spec -surf_A NodeList.1D -met HEAT_05   \\\n"
"                  -input in.1D -fwhm 8 -add_index         \\\n"
"                  -output in_smh8.1D.dset \n"
/*              "      Or using the older (less recommended method):\n"
"      SurfSmooth  -spec quick.spec -surf_A NodeList.1D -met LB_FEM   \\\n"
"                  -input in.1D -Niter 100 -fwhm 8 -add_index         \\\n"
"                  -output in_sm8.1D.dset \n"
"         This command filters (on the surface) the data in in.1D\n"
"         and puts the output in in_sm8.1D.dset with the first column \n"
"         containing the node index and the second containing the \n"
"         filtered version of in.1D.\n"
"         \n"*/
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
"\n", sio, sm,  st, s); 
       SUMA_free(s); s = NULL; SUMA_free(st); 
       st = NULL; SUMA_free(sm); sm = NULL; SUMA_free(sio); sio = NULL;
       s = SUMA_New_Additions(0, 1); printf("%s\n", s);SUMA_free(s); s = NULL;
       printf("       Ziad S. Saad SSCC/NIMH/NIH saadz@mail.nih.gov     \n");
       exit (0);
   }


#define SURFSMOOTH_MAX_SURF 1  /*!< Maximum number of input surfaces */

typedef enum { SUMA_NO_METH,  SUMA_LB_FEM_1D, SUMA_LB_FEM, SUMA_LM, SUMA_BRUTE_FORCE, 
                              SUMA_NN_GEOM, SUMA_HEAT_05_1D, SUMA_HEAT_05_Pre_07,
                              SUMA_HEAT_07} SUMA_SMOOTHING_METHODS;

typedef struct {
   float OffsetLim;
   float lim;
   double fwhm;
   double tfwhm;
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
   char *master_name;
   char *out_name;   /* this one's dynamically allocated so you'll have to free it yourself */
   char *ShowOffset_DBG; /* meant to be a file where one outputs some debugging info. Not being used ...*/
   char *surf_out;
   char *surf_names[SURFSMOOTH_MAX_SURF];
   char *spec_file;
   int MatchMethod;
   byte *nmask;
   byte strict_mask;
   unsigned int rseed;
   double sigma;
   char FWHM_mixmode[20];
   
   int overwrite;
   int detrend_master;
   int detpoly_master;
   char *detprefix_master;
   int detrend_in;
   int detpoly_in;
   char *detprefix_in;
   int bmall;
   
   int debug;
   
   byte scaleinput;
   byte scalemaster;
   SUMA_DSET_FORMAT oform;
   SUMA_GENERIC_ARGV_PARSE *ps;
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
SUMA_SURFSMOOTH_OPTIONS *SUMA_SurfSmooth_ParseInput (
   char *argv[], int argc, SUMA_GENERIC_ARGV_PARSE *ps)
{
   static char FuncName[]={"SUMA_SurfSmooth_ParseInput"}; 
   SUMA_SURFSMOOTH_OPTIONS *Opt=NULL;
   int kar, i, ind, exists=0;
   char *outname = NULL, *ooo=NULL;
   SUMA_Boolean brk = NOPE;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   Opt = (SUMA_SURFSMOOTH_OPTIONS *)
            SUMA_malloc(sizeof(SUMA_SURFSMOOTH_OPTIONS));
   
   kar = 1;
   Opt->OffsetLim = -1.0;
   Opt->MatchMethod = 0;
   Opt->lim = 1000000.0;
   Opt->fwhm = -1;
   Opt->tfwhm = -1;
   Opt->ShowNode = -1;
   Opt->Method = SUMA_NO_METH;
   Opt->rseed = 123456;
   Opt->dbg = 0;
   Opt->if_name = NULL;
   Opt->if_name2 = NULL;
   Opt->in_name = NULL;
   Opt->master_name = NULL;
   Opt->out_name = NULL;
   Opt->vp_name = NULL; 
   Opt->sv_name = NULL;
   Opt->surf_out = NULL;
   Opt->ShowOffset_DBG = NULL;
   Opt->iType = SUMA_FT_NOT_SPECIFIED;
   Opt->N_iter = -100;
   Opt->kpb = -1.0;
   Opt->l = -1.0;
   Opt->m = -1.0;
   Opt->AddIndex = 0;
   Opt->insurf_method = 0;
   Opt->nmask = NULL;
   Opt->spec_file = NULL;
   Opt->sigma = -1.0;
   Opt->oform = SUMA_NO_DSET_FORMAT;
   Opt->strict_mask = 1;
   sprintf(Opt->FWHM_mixmode,"arit");
   Opt->ps = ps;
   SUMA_Set_Taubin_Weights(SUMA_EQUAL);
   Opt->detrend_master = -1;
   Opt->detpoly_master = -2;
   Opt->detprefix_master = NULL;
   Opt->detrend_in = -2;
   Opt->detpoly_in = -2;
   Opt->detprefix_in = NULL;
   Opt->scaleinput = 0;
   Opt->scalemaster = 1;
   Opt->debug = 0;
   for (i=0; i<SURFSMOOTH_MAX_SURF; ++i) { Opt->surf_names[i] = NULL; }
   outname = NULL; ooo=NULL;
   exists = 0;
   Opt->overwrite = 0;
   Opt->bmall = 0;
	brk = NOPE;
	while (kar < argc) { /* loop accross command ine options */
		/*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
		if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
			 usage_SUMA_SurfSmooth(ps);
          exit (0);
		}
		
		SUMA_SKIP_COMMON_OPTIONS(brk, kar);

      if (!brk && (strcmp(argv[kar],"-detrend_master") == 0 )){         
         Opt->detrend_master = -1 ;
         if( kar < argc-1 && isdigit(argv[kar+1][0]) ){
            Opt->detrend_master = (int)strtod(argv[++kar],NULL) ;
            if( Opt->detrend_master == 0 ){ /* Use poly of order 0 (mean) */
              Opt->detpoly_master = 0 ; 
              fprintf(SUMA_STDOUT,"-detrend_master 0 replaced by -detpoly_master 0\n") ;
              Opt->detrend_master = -2;
            }
         }
         brk = YUP;
      }
      if (!brk && (strcmp(argv[kar],"-detpoly_master") == 0 )){         
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need a value after -detpoly_master \n");
            exit (1);
         }
         Opt->detpoly_master = (int)strtod(argv[++kar],NULL) ;
         brk = YUP;
      }
      if (!brk && (strcmp(argv[kar], "-no_detrend_master") == 0)) {
			Opt->detrend_master = -2;
			brk = YUP;
		}
      if (!brk && (strcmp(argv[kar], "-bmall") == 0)) {
			Opt->bmall = 1;
			brk = YUP;
		}
      if( strcmp(argv[kar],"-detprefix_master") == 0 ){
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need a value after -detprefix_master \n");
            exit (1);
         }
         Opt->detprefix_master = argv[++kar];
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar],"-detrend_in") == 0 )){         
         Opt->detrend_in = -1 ;
         if( kar < argc-1 && isdigit(argv[kar+1][0]) ){
            Opt->detrend_in = (int)strtod(argv[++kar],NULL) ;
            if( Opt->detrend_in == 0 ){ /* Use poly of order 0 (mean) */
              Opt->detpoly_in = 0 ; 
              fprintf(SUMA_STDOUT,"-detrend_in 0 replaced by -detpoly_in 0\n") ;
              Opt->detrend_in = -2;
            }
         }
         brk = YUP;
      }
      if (!brk && (strcmp(argv[kar],"-detpoly_in") == 0 )){         
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need a value after -detpoly_in \n");
            exit (1);
         }
         Opt->detpoly_in = (int)strtod(argv[++kar],NULL) ;
         brk = YUP;
      }
      if (!brk && (strcmp(argv[kar], "-no_detrend_in") == 0)) {
			Opt->detrend_in = -2;
			brk = YUP;
		}
      if( strcmp(argv[kar],"-detprefix_in") == 0 ){
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need a value after -detprefix_in \n");
            exit (1);
         }
         Opt->detprefix_in = argv[++kar];
         brk = YUP;
      }
      
      if (!brk && strcmp(argv[kar], "-dist") == 0)
		{
			kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -dist \n");
				exit (1);
			}
			Opt->OffsetLim = atof(argv[kar]);
         if (Opt->OffsetLim <= 0 && Opt->OffsetLim != -1.0) {
            fprintf (SUMA_STDERR, "Bad value (%f) for refresh_rate\n", Opt->OffsetLim);
				exit (1);
         }

			brk = YUP;
		}
     
      if (!brk && (strcmp(argv[kar], "-debug") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need 1 argument after -debug \n");
				exit (1);
			}
			Opt->debug = atoi(argv[kar]); 
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-dbg_n") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need 1 argument after -dbg_n \n");
				exit (1);
			}
         if (!Opt->debug) Opt->debug = 1;
			Opt->ShowNode = atoi(argv[kar]); 
         SUMA_Set_SurfSmooth_NodeDebug(Opt->ShowNode);
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
      if (!brk && (strcmp(argv[kar], "-rseed") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need 1 positive integer after -rseed \n");
				exit (1);
			}
			Opt->rseed = atoi(argv[kar]);
         if (Opt->rseed < 0) {
            SUMA_S_Errv("-rseed needs a +ve integer. Have %d\n", Opt->rseed);
            exit(1);
         }
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
            fprintf (SUMA_STDERR, 
                     "Weights option %s not understood.\n", 
                     argv[kar]);
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
      
      if (!brk && (strcmp(argv[kar], "-blurmaster") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -blurmaster \n");
				exit (1);
			}
			Opt->master_name = argv[kar];
			brk = YUP;
		}
      
      
      if (!brk && (strcmp(argv[kar], "-output") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, 
                     "need argument after -output\n");
				exit (1);
			}
			if (Opt->surf_out) {
            fprintf (SUMA_STDERR, 
                     "options -surf_out and -output are mutually exclusive\n");
				exit (1);
         }
         outname = argv[kar];
			brk = YUP;
		}
      #if 0 /* -overwrite now processed secretly by mainENTRY(); */
         
      if (!brk && (strcmp(argv[kar], "-overwrite") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -overwrite\n");
				exit (1);
			}
			if (Opt->surf_out || outname) {
            fprintf (SUMA_STDERR, "options -surf_out, -output, and -ovewrite are mutually exclusive\n");
				exit (1);
         }
         outname = argv[kar];
         Opt->overwrite = 1;
			brk = YUP;
		}
      #endif
      if (!brk && (strcmp(argv[kar], "-add_index") == 0)) {
			Opt->AddIndex = 1;
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-use_neighbors_outside_mask") == 0)) {
			Opt->strict_mask = 0;
			brk = YUP;
		}
      
      
      #if 0    /* Now handled in default parsing */
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
      
      #endif
      
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
      if (!brk && (strcmp(argv[kar], "-target_fwhm") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -target_fwhm \n");
				exit (1);
			}
			Opt->tfwhm = atof(argv[kar]);
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
         else if (strcmp(argv[kar], "HEAT_05") == 0)  Opt->Method = SUMA_HEAT_05_Pre_07;
         else if (strcmp(argv[kar], "HEAT_07") == 0)  Opt->Method = SUMA_HEAT_07;
         else if (strcmp(argv[kar], "HEAT") == 0)  {
            fprintf (SUMA_STDERR,"Option HEAT has been changed considerably.\n"
                                 "It is recommended you use HEAT_07 or HEAT_05\n"
                                 "for backward compatibility.\n");
            exit(1);
         } else {
            fprintf (SUMA_STDERR, "Method %s not supported.\n", argv[kar]);
				exit (1);
         }
         if (Opt->Method == SUMA_HEAT_05_Pre_07 || Opt->Method == SUMA_HEAT_07) {
            if (Opt->N_iter < 0) {
               Opt->N_iter = -1;
            }
         } else {
            if (Opt->N_iter < 0) {
               Opt->N_iter = -Opt->N_iter;
            }  
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
   Opt->overwrite = THD_ok_overwrite();

   /* check on options for HEAT budiness first */
   if (Opt->Method == SUMA_HEAT_07) {
      if (Opt->N_iter < 0 &&  (Opt->fwhm > 0 || Opt->tfwhm > 0) && Opt->sigma < 0)  {
         /* will make a suggestion, but after surface is read */
      } else if (Opt->N_iter < 0 && Opt->tfwhm && Opt->fwhm < 0 && Opt->sigma < 0)  {
         fprintf (SUMA_STDERR,"Error %s:\n"
                              "All blurring parameters unspecified.\n", FuncName);
         exit (1);
      } else if (Opt->N_iter > 0 &&  (Opt->fwhm > 0 || Opt->tfwhm > 0) &&   Opt->sigma > 0)  { 
         fprintf (SUMA_STDERR,"Error %s:\n"
                              "All three parameters specified.\n", FuncName);
         exit (1);
      } else if (Opt->N_iter > 0 &&  (Opt->fwhm > 0 || Opt->tfwhm > 0) ) {
         /* will work this one later */
      } else if (Opt->N_iter > 0 && Opt->sigma > 0) {
         /* this one is OK too. */
      } else if ((Opt->fwhm > 0 || Opt->tfwhm > 0) &&  Opt->sigma > 0) {
         /* this one is OK too. */
      } else {
         fprintf (SUMA_STDERR,"Error %s:\n"
                              "Unexpected combo.\n", FuncName);
         exit (1);
      }
   }
   
   if (Opt->Method == SUMA_HEAT_05_1D || Opt->Method == SUMA_HEAT_05_Pre_07) {
      float sequiv;
      if (Opt->N_iter < 0 &&  Opt->fwhm > 0 && Opt->sigma < 0)  {
         /* will make a suggestion, but after surface is read */
         
      }else {
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
            /* recalculate sigma */
            sequiv = Opt->fwhm * 0.42466090;
            Opt->sigma = sequiv / sqrt(Opt->N_iter);
         }
         if (Opt->fwhm < 0) {
            sequiv = sqrt(Opt->N_iter)*Opt->sigma;
            Opt->fwhm = sequiv / 0.42466090;
         }
         if (Opt->sigma < 0) {
            sequiv = Opt->fwhm * 0.42466090;
            Opt->sigma = sequiv / sqrt(Opt->N_iter);
         }
      }
   }
   
   if (Opt->Method != SUMA_HEAT_05_Pre_07 && Opt->Method != SUMA_HEAT_07) {
      if (Opt->N_iter == -1) { /* default */ Opt->N_iter = 100; }
      if (Opt->N_iter < 1) {
         fprintf (SUMA_STDERR,"Error %s:\nWith -Niter N option, N must be > 1\n", FuncName);
         exit (1);
      }
   } else {
      if (Opt->N_iter == -1) {Opt->N_iter = -2; }/* let it slide, we'll take care of it after surface is loaded in main */
   }
   
   if ( (Opt->N_iter % 2) &&
        (Opt->Method == SUMA_LB_FEM_1D || Opt->Method == SUMA_LB_FEM || 
        Opt->Method == SUMA_HEAT_05_1D  ||
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
     
   if (Opt->insurf_method == 2) {
      if (!Opt->surf_names[0] || !Opt->spec_file) {
         fprintf (SUMA_STDERR,   "failed to specify either -spec or -surf_X options.\n");
         exit(1);  
      }
   }
    
   if (outname) {
      if (SUMA_filexists(outname) && !Opt->overwrite) {
         fprintf (SUMA_STDERR,"Error %s:\noutput file %s exists.\n", FuncName, outname);
         exit(1);
      }
      Opt->out_name = SUMA_copy_string(outname);
      Opt->oform = SUMA_GuessFormatFromExtension(Opt->out_name, Opt->in_name);
      SUMA_LHv("Format with %s, %s is %d\n", Opt->out_name, Opt->in_name, Opt->oform);
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
            Opt->oform = SUMA_GuessFormatFromExtension(Opt->in_name, NULL);
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
         case SUMA_HEAT_05_Pre_07:
            /* form autoname  */
            Opt->oform = SUMA_GuessFormatFromExtension(Opt->in_name, NULL);
            Opt->out_name = SUMA_RemoveDsetExtension_s(Opt->in_name, Opt->oform);
            Opt->out_name = SUMA_append_replace_string(Opt->out_name,"_sm", "", 1); /* add _sm to prefix */
            Opt->out_name = SUMA_append_replace_string(Opt->out_name, (char*)SUMA_ExtensionOfDsetFormat (Opt->oform), "", 1); /* add extension */
            break;
         case SUMA_HEAT_07:
            /* form autoname  */
            Opt->oform = SUMA_GuessFormatFromExtension(Opt->in_name, NULL);
            Opt->out_name = SUMA_RemoveDsetExtension_s(Opt->in_name, Opt->oform);
            Opt->out_name = SUMA_append_replace_string(Opt->out_name,"_smh7", "", 1); /* add _sm to prefix */
            Opt->out_name = SUMA_append_replace_string(Opt->out_name, (char*)SUMA_ExtensionOfDsetFormat (Opt->oform), "", 1); /* add extension */
            break;
         default:
            fprintf (SUMA_STDERR,"Error %s:\nNot ready for this option here.\n", FuncName);
            exit(1);
            break;
      }
      
   }
   
   if (  Opt->Method != SUMA_LM && Opt->Method != SUMA_BRUTE_FORCE &&
         Opt->Method != SUMA_NN_GEOM ) {
      exists = SUMA_WriteDset_NameCheck_s (Opt->out_name, NULL, 
                                           Opt->oform, 0, &ooo);
      if (exists != 0 && !Opt->overwrite) {
         SUMA_S_Errv("Output dataset %s exists.\n", ooo);
         SUMA_free(ooo); ooo=NULL;
         exit(1);
      }
   }

   if (Opt->tfwhm > -1  && Opt->Method != SUMA_HEAT_07) {
      SUMA_S_Err("Cannot use -target_fwhm with anything but the HEAT_07 method.\n");
      exit(1);
   }
   
   if (Opt->N_iter > 0 && Opt->sigma > 0 && Opt->master_name) {
      SUMA_S_Err("Cannot use both of -Niter and -sigma with -blurmaster.\n"
                 "What's  the point in doing so since Niter and sigma \n"
                 "together fully determine the smoothing process.\n");
      exit(1);
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
      case SUMA_HEAT_05_Pre_07:
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
      case SUMA_HEAT_07:
         if (!Opt->in_name) {
            fprintf (SUMA_STDERR,"Error %s:\ninput data not specified.\n", FuncName);
            exit(1);
         }
         if (Opt->fwhm ==  -1.0 && Opt->tfwhm == -1.0 && !(Opt->sigma > 0 && Opt->N_iter > 0)) {
            fprintf (SUMA_STDERR,"Error %s:\n-fwhm or -target_fwhm option must be used with -met HEAT.\n", FuncName); 
            exit(1);
         }else if (Opt->fwhm !=  -1.0 && Opt->tfwhm != -1.0) {
            fprintf (SUMA_STDERR,"Error %s:\n-fwhm and -target_fwhm options are mutually exclusive.\n", FuncName); 
            exit(1);
         }
         if (Opt->fwhm > 0.0) {  
            if (0 && Opt->master_name) { /* I don't see why ... */
               SUMA_S_Err("No use for -master_name option without -target_fwhm");
               exit(1);
            }
         }
         
         if (Opt->tfwhm > 0.0) {
            if (Opt->N_iter > 0 && Opt->sigma != -1.0) {
               SUMA_S_Err("With -target_fwhm, you cannot specify both of -niter and -sigma\n");
               exit(1);
            }
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
   
   if (0 && ((Opt->ps->bmaskname && Opt->ps->nmaskname) || (Opt->ps->bmaskname && Opt->ps->cmask) || (Opt->ps->nmaskname && Opt->ps->cmask) ) ) {
      fprintf (SUMA_STDERR,"Error %s:\n-n_mask, -b_mask, and -c_mask options are mutually exclusive.\n", FuncName);
      exit(1);
   }else {
      /* SUMA_S_Warn("For testing! Turn me back on!!!\n"); */ /* Now it is allowed */
   }
   SUMA_RETURN (Opt);
}

int main (int argc,char *argv[])
{/* Main */    
   static char FuncName[]={"SurfSmooth"}; 
	int kar, icol, nvec, ncol=0, i, ii, N_inmask = -1;
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
   SUMA_GENERIC_ARGV_PARSE *ps=NULL;
   SUMA_DSET *dset = NULL;
   SUMA_SurfSpecFile *Spec = NULL;
   int N_Spec=-1;
   int iform;
   char *ooo=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_STANDALONE_INIT;
	SUMA_mainENTRY;
   
   
	/* Allocate space for DO structure */
	SUMAg_DOv = SUMA_Alloc_DisplayObject_Struct (SUMA_MAX_DISPLAYABLE_OBJECTS);
   ps = SUMA_Parse_IO_Args(argc, argv, "-o;-i;-t;-spec;-s;-sv;-talk;-mask;");
   
   if (argc < 6)
       {
          usage_SUMA_SurfSmooth(ps);
          exit (1);
       }
   
   Opt = SUMA_SurfSmooth_ParseInput (argv, argc, ps);
   cs = ps->cs;
   if (!cs) exit(1);
   
   if (Opt->debug > 2) LocalHead = YUP;
   
   if (Opt->debug) SUMA_S_Note("Reading surface(s)\n");

   Spec = SUMA_IO_args_2_spec(ps, &N_Spec);
   if (N_Spec == 0) {
      SUMA_S_Err("No surfaces found.");
      exit(1);
   }

   SUMA_LH("Loading surface...");
   SO = SUMA_Load_Spec_Surf(Spec, 0, ps->sv[0], Opt->debug);
   if (!SUMA_SurfaceMetrics_eng (SO, "EdgeList, MemberFace", 
                                 NULL, Opt->debug, SUMAg_CF->DsetList)) {
      SUMA_S_Err("Failed in SUMA_SurfaceMetrics.\n");
      exit(1);
   }
   if (!SO) {
      SUMA_S_Err("Failed to read input surface \n");
      exit (1);
   }

   /* setup the mask, if needed */
   if (Opt->nmask) {
      SUMA_S_Err("Should be null here!");
      exit(1);
   }
   
   if (Opt->debug) SUMA_S_Note("Considering masks, if any are specified.");
   if (!(Opt->nmask = 
            SUMA_load_all_command_masks(  Opt->ps->bmaskname, 
                                          Opt->ps->nmaskname, 
                                          Opt->ps->cmask, 
                                          SO->N_Node, &N_inmask)) 
         && N_inmask < 0) {
         SUMA_S_Err("Failed loading mask");
         exit(1);
   }
   
   if (Opt->nmask) {
      if (Opt->debug) fprintf(SUMA_STDOUT,"%d nodes in mask:\n", N_inmask);
      if (LocalHead) {
         ii = 0;
         for (i=0; i<SO->N_Node; ++i) {
            if (Opt->nmask[i]) {
               fprintf(SUMA_STDERR,"%6d\n", i); ++ii;
               if (!(ii % 12)) fprintf(SUMA_STDERR,"\n");
            }
         }
         fprintf(SUMA_STDERR,"\n"); 
      }
      if (Opt->debug > 1){
         char *stmp=SUMA_append_replace_string(
                  "surfsmooth_mask_", ".1D", Opt->out_name, 0);
         SUMA_S_Notev("Wrote mask to %s\n", stmp);
         SUMA_WRITE_ARRAY_1D(Opt->nmask,SO->N_Node,1,stmp);
         SUMA_free(stmp); stmp = NULL;
      }
   } else {
      if (Opt->debug) {
         SUMA_S_Note("No masking.");
      }
   }
   
   if (Opt->ShowNode >= 0 && Opt->ShowNode >= SO->N_Node) {
      fprintf (SUMA_STDERR,
               "Error %s: Requesting debugging info for a node index (%d) \n"
               "that does not exist in a surface of %d nodes.\n"
               "Remember, indexing starts at 0.\n", 
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
      if (Opt->debug) SUMA_S_Note("Setting up communication");
      if (  Opt->Method == SUMA_LB_FEM_1D || 
            Opt->Method == SUMA_LB_FEM || 
            Opt->Method == SUMA_HEAT_05_1D || 
            Opt->Method == SUMA_HEAT_05_Pre_07 ||
            Opt->Method == SUMA_HEAT_07) { 
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
  
   ncol = 3; /* default for geometry smoothing. 
               That is changed below for data smoothing. */
   switch (Opt->Method) {
      case SUMA_HEAT_05_1D:/* Operates on 1D files, OBSOLETE but still */
                           /* accessible with -met HEAT_1D */
         /* Moo Chung's method for interpolation weights */
         {
            if (Opt->debug) SUMA_S_Note("HEAT_05_1D method");
            if (Opt->N_iter < 0) {
               SUMA_S_Errv("Bad number of iterations (%d)\n", Opt->N_iter);
               exit(1);
            }
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
                                    "                    found %d rows"
                                    " instead.\n",
                                     FuncName, SO->N_Node, nvec);
               exit(1); 
            }
            if (LocalHead) SUMA_etime(&start_time,0);
            if (Opt->debug) SUMA_S_Note("Calculating weights");
            wgt = SUMA_Chung_Smooth_Weights_05_single(SO, Opt->sigma);
            if (!wgt) {
               SUMA_SL_Err("Failed to compute weights.\n");
               exit(1);
            }
            
            if (LocalHead) {
               etime_GetOffset = SUMA_etime(&start_time,1);
               fprintf(SUMA_STDERR, 
                  "%s: weight computation took %f seconds for %d nodes.\n"
                  "Projected time per 100000 nodes is: %f minutes\n", 
                     FuncName, etime_GetOffset, SO->N_Node, 
                        etime_GetOffset * 100000 / 60.0 / (SO->N_Node));
            }
            
            if (Opt->debug) SUMA_S_Note("HEAT_05_1D smoothing");
            dsmooth = SUMA_Chung_Smooth_05 ( 
                        SO, wgt, Opt->N_iter, Opt->fwhm, far, ncol, 
                        SUMA_COLUMN_MAJOR, NULL, cs, Opt->nmask,
                        Opt->strict_mask);
            
            if (LocalHead) {
               etime_GetOffset = SUMA_etime(&start_time,1);
               fprintf(SUMA_STDERR, 
                  "%s: Total processing took %f seconds for %d nodes.\n"
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
            if (Opt->debug) SUMA_S_Note("LB_FEM_1D method");
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
            if (Opt->debug) SUMA_S_Note("Calculating weights");
            wgt = SUMA_Chung_Smooth_Weights(SO);
            if (!wgt) {
               SUMA_SL_Err("Failed to compute weights.\n");
               exit(1);
            }
            
            if (LocalHead) {
               etime_GetOffset = SUMA_etime(&start_time,1);
               fprintf(SUMA_STDERR, 
                  "%s: weight computation took %f seconds for %d nodes.\n"
                  "Projected time per 100000 nodes is: %f minutes\n", 
                     FuncName, etime_GetOffset, SO->N_Node, 
                     etime_GetOffset * 100000 / 60.0 / (SO->N_Node));
            }
            
            if (Opt->debug) SUMA_S_Note("LB_FEM_1D smoothing");
            dsmooth = SUMA_Chung_Smooth ( 
                  SO, wgt, Opt->N_iter, Opt->fwhm, far, ncol, 
                  SUMA_COLUMN_MAJOR, NULL, cs, Opt->nmask, Opt->strict_mask);
            
            if (LocalHead) {
               etime_GetOffset = SUMA_etime(&start_time,1);
               fprintf(SUMA_STDERR, 
                     "%s: Total processing took %f seconds for %d nodes.\n"
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
            if (Opt->debug) SUMA_S_Note("LB_FEM method");
            /* now load the input data */
            iform = SUMA_GuessFormatFromExtension(Opt->in_name, NULL);
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
            if (Opt->debug) SUMA_S_Note("Calculating weights");
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
            
            if (Opt->debug) SUMA_S_Note("LB_FEM smoothing");
            if (!SUMA_Chung_Smooth_dset ( SO, wgt, 
                                          Opt->N_iter, Opt->fwhm, 
                                          dset, cs, Opt->nmask, Opt->strict_mask)) {
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
         
      case SUMA_HEAT_07:
         /* Controlled Moo Chung's method for interpolation weights */
         {
            double avg_wt, sequiv, sigma;
            double fwhmttt;
            double fwhm_in_start = -1.0, fwhm_master_start = -1.0;
            double **wgtd=NULL;
            float *fr = NULL;
            float *fwhmv=NULL, *fwhmhist=NULL;
            int  *icols, N_icols = -1, N;
            int nref_in=-1 , nref_master = -1, jj,iv,kk ;
            float **ref_in=NULL  , **ref_master = NULL, tm,fac,fq ;            
            SUMA_DSET *master_dset=NULL;
            MRI_IMARR *corder_inar=NULL ;
            THD_3dim_dataset *newset=NULL, *inset=NULL ;
            
            if (Opt->debug) SUMA_S_Note("HEAT_07 method");
            
            /* load dset */
            iform = SUMA_GuessFormatFromExtension(Opt->in_name, NULL);
            if (!(dset = SUMA_LoadDset_s (Opt->in_name, &iform, Opt->debug))) {
               SUMA_S_Err("Failed to read  dset");
               exit(1);
            }
            if (LocalHead) {
               SUMA_LHv("Input %s:\n", Opt->in_name);
               /* SUMA_ShowDset(dset, 0, NULL); */
            }  
            if (Opt->oform == SUMA_NO_DSET_FORMAT) Opt->oform = iform;

            if (!SDSET_VECLEN(dset) || !SDSET_VECNUM(dset)) {
               SUMA_SL_Err("Empty file");
               exit(1);
            }
            if (SDSET_VECLEN(dset) != SO->N_Node) {
               if (LocalHead) fprintf(SUMA_STDERR, 
                  "Warning %s:\n"
                  "Expecting 1D file to have %d rows\n"
                  "                    found %d rows instead.\n"
                  "Function should deal with this properly but check results\n",
                   FuncName, SO->N_Node, SDSET_VECLEN(dset));
            }
            
            if( Opt->detrend_in == -1 ) 
               Opt->detrend_in = SDSET_VECNUM(dset) / 30 ;
            if(   Opt->detrend_in >= 0 && 
               2*Opt->detrend_in+3 >= SDSET_VECNUM(dset) ){
               SUMA_S_Errv(   "-detrend_in %d is too big for"
                              " this dataset",Opt->detrend_in) ;
            }

            if (Opt->AddIndex || Opt->oform == SUMA_NIML) {
               if (!SUMA_AddNodeIndexColumn(dset, SO->N_Node)) {
                  SUMA_S_Err("Failed to add a node index column");
                  exit(1);
               }
            }
            
            if (Opt->fwhm > 0 && Opt->tfwhm > 0) {
               SUMA_S_Err("Logic error, should not be here.");
               exit(1);
            }
            
            if (Opt->detrend_in > -2 && Opt->detpoly_in > -1) {
               SUMA_S_Err("detrend_in & detpoly_in are mutually exclusive.");
               exit(1);
            }
            
            if (Opt->detrend_in > -2 || Opt->detpoly_in > -1) {
               /* there is work to do detrending the input */
               
               /* need to take dataset to AFNI dset */
               
               if (!(inset = SUMA_sumadset2afnidset(&dset, 1, 1))) {
                  SUMA_S_Err("Failed to transform surface dset to afni dset");
                  exit(1);
               }
               if (Opt->detrend_in > 0) {
                  if (Opt->debug) SUMA_S_Notev(
                                    "Detrending input %d\n",
                                     Opt->detrend_in);
                  nref_in = 2*Opt->detrend_in+3;
                  SUMA_LHv("trig. detrending start: "
                           "%d baseline funcs, %d time points\n"
                           ,nref_in, DSET_NVALS(inset)) ;
                  ref_in = THD_build_trigref( Opt->detrend_in ,
                                              DSET_NVALS(inset) ) ;
                  if( ref_in == NULL ) ERROR_exit("THD_build_trigref failed!") ;
               } else {
                  if (Opt->debug) SUMA_S_Notev(
                           "Detrending input with poly %d\n",
                            Opt->detrend_in);
                  nref_in = Opt->detpoly_in+1;
                  SUMA_LHv("poly. detrending start: "
                           "%d baseline funcs, %d time points\n",
                           nref_in, DSET_NVALS(inset)) ;
                  ref_in = THD_build_polyref( nref_in , DSET_NVALS(inset) ) ;
                  if( ref_in == NULL ) ERROR_exit("THD_build_trigref failed!") ;

               }

               if (!(newset = THD_detrend_dataset( inset , nref_in ,
                                                   ref_in , 2 , 
                                                   Opt->scaleinput , 
                                                   Opt->nmask , &corder_inar )))
               { 
                  SUMA_S_Err("detrending failed!") ;
                  exit(1);
               }
      
               SUMA_LH("detrending of input done") ;
               DSET_delete(inset); inset=newset; newset = NULL;
               
               /* Now back to SUMA_DSET */
               dset = SUMA_afnidset2sumadset(&inset, 1, 1); /* don't need afni
                                                               volume anymore */
               /* write out detrended input? */
               if (Opt->detprefix_in) {
                  /* Add the history line */
                  if (!SUMA_AddNgrHist (dset->ngr, FuncName, argc, argv)) {
                     SUMA_SL_Err("Failed in SUMA_AddNgrHist");
                  }
                  SUMA_LHv("About to write detrended input dset %s, oform %d\n", 
                           Opt->out_name, Opt->oform);
                  ooo = SUMA_WriteDset_s( Opt->detprefix_in, dset, Opt->oform, 
                                          Opt->overwrite, 0);
                  SUMA_free(ooo); ooo=NULL;
               }
            }
            
            if (Opt->master_name) {
               if (Opt->debug) {
                  SUMA_LHv("Master Processing %s:\n", Opt->master_name);
               }                
               SUMA_LHv("Have master %s (detrend %d, poly %d)\n", 
                        Opt->master_name, Opt->detrend_master,
                        Opt->detpoly_master);
               /* get the master */
               iform = SUMA_GuessFormatFromExtension(Opt->master_name, NULL);
               if (!(master_dset = 
                  SUMA_LoadDset_s (Opt->master_name, &iform, 0))) {
                  SUMA_S_Err("Failed to read master dset");
                  exit(1);
               }
               if (!SDSET_VECLEN(master_dset) || !SDSET_VECNUM(master_dset)) {
                  SUMA_SL_Err("Empty file");
                  exit(1);
               }
               if (SDSET_VECLEN(master_dset) != SO->N_Node) {
                  if (LocalHead) fprintf(SUMA_STDERR, 
                        "Warning %s:\n"
                        "Expecting 1D file to have %d rows\n"
                        "                    found %d rows instead.\n"
                        "Function should deal with this properly \n"
                        "but check results\n",
                        FuncName, SO->N_Node, SDSET_VECLEN(master_dset));
               }

               if (Opt->AddIndex || Opt->oform == SUMA_NIML) {
                  if (!SUMA_AddNodeIndexColumn(master_dset, SO->N_Node)) {
                     SUMA_S_Err("Failed to add a node index column");
                     exit(1);
                  }
               }

               if( Opt->detrend_master == -1 ) {
                  Opt->detrend_master = SDSET_VECNUM(master_dset) / 30 ;
                  if (Opt->detrend_master < 1) {
                     if (Opt->debug) 
                        SUMA_S_Notev("Zero order polynomial detrending"
                                     "chosen for %s\n", Opt->master_name); 
                     Opt->detrend_master = -1;
                     Opt->detpoly_master = 0;
                  } else {
                     if (Opt->debug) 
                        SUMA_S_Notev("detrend %d chosen for %s\n", 
                           Opt->detrend_master, Opt->master_name); 
                  }
               }
               if(   Opt->detrend_master >= 0 && 
                     2*Opt->detrend_master+3 >= SDSET_VECNUM(master_dset) ){
                  SUMA_S_Errv("-detrend_master %d is too big for this"
                              "dataset",Opt->detrend_master) ;
               }

               if (Opt->detrend_master > 0 || Opt->detpoly_master > -1) {
                  /* there is work to do detrending the master */

                  /* need to take dataset to AFNI master_dset */
                  if (!(inset = SUMA_sumadset2afnidset(&master_dset, 1, 1))) {
                     SUMA_S_Err( "Failed to transform surface "
                                 "master_dset to afni master_dset");
                     exit(1);
                  }

                  if (Opt->detrend_master > 0) {
                     if (Opt->debug) 
                        SUMA_S_Notev("Detrending master %d\n",
                              Opt->detrend_master);
                     nref_master = 2*Opt->detrend_master+3;
                     SUMA_LHv("trig. detrending start: "
                              "%d baseline funcs, %d time points\n",
                              nref_master, DSET_NVALS(inset)) ;
                     ref_master = THD_build_trigref( Opt->detrend_master ,
                                                DSET_NVALS(inset) ) ;
                     if( ref_master == NULL ) 
                        ERROR_exit("THD_build_trigref failed!") ;
                  } else {
                     if (Opt->debug) 
                        SUMA_S_Notev("Detrending master with poly %d\n",
                                    Opt->detrend_master);
                     nref_master = Opt->detpoly_master+1;
                     SUMA_LHv("poly. detrending start: %d baseline funcs,"
                              " %d time points\n",
                              nref_master, DSET_NVALS(inset)) ;
                     ref_master = THD_build_polyref( nref_master ,
                                              DSET_NVALS(inset) ) ;
                     if( ref_master == NULL ) 
                        ERROR_exit("THD_build_trigref failed!") ;

                  }

                  if (!(newset = THD_detrend_dataset( 
                              inset , nref_master , ref_master , 2 ,
                              Opt->scalemaster , Opt->nmask , NULL ))) { 
                     SUMA_S_Err("detrending failed!") ;
                     exit(1);
                  }

                  SUMA_LH("detrending of master done") ;
                  DSET_delete(inset); inset=newset; newset = NULL;
                  for(jj=0;jj<nref_master;jj++) 
                     free(ref_master[jj]) ; 
                  free(ref_master); 

                  /* Now back to SUMA_DSET, and kill afni volume*/
                  master_dset = SUMA_afnidset2sumadset(&inset, 1, 1); 
                  /* write out detrended master? */
                  if (Opt->detprefix_master) {
                     /* Add the history line */
                     if (!SUMA_AddNgrHist (master_dset->ngr, FuncName, 
                                           argc, argv)) {
                        SUMA_SL_Err("Failed in SUMA_AddNgrHist");
                     }
                     SUMA_LHv("About to write detrended master dset %s, "
                              "oform %d\n", 
                              Opt->out_name, Opt->oform);
                     ooo = SUMA_WriteDset_s( Opt->detprefix_master, master_dset, 
                                             Opt->oform, 
                                             Opt->overwrite, 0);
                     SUMA_free(ooo); ooo=NULL;
                  }
                  
                  if (!Opt->bmall) { /* a la 3dBlurToFWHM */
                     int ntouse, idel, ibot, cnt, ibm;
                     SUMA_DSET *ndset=NULL;
                     byte *colmask=NULL;
                     
                     ntouse = SUMA_MIN_PAIR(SDSET_VECNUM(master_dset), 32);
                     idel = SDSET_VECNUM(master_dset)/ntouse;
                     ibot = (SDSET_VECNUM(master_dset)-1 - idel*(ntouse-1)) / 2;
                     if (Opt->debug) SUMA_S_Notev(
                        "Using blurmaster sub-bricks [%d..%d(%d)]\n",
                              ibot, ibot+(ntouse-1)*idel, idel);
                     colmask = (byte *)SUMA_calloc(
                                 SDSET_VECNUM(master_dset),sizeof(byte));
                     cnt = 0;
                     for ( ibm=ibot ; ibm < SDSET_VECNUM(master_dset) && 
                           cnt < ntouse  ; ibm+=idel ) {
                        colmask[ibm] = 1; ++cnt;
                     }
                     if (!(ndset = SUMA_MaskedCopyofDset(master_dset, 
                                       NULL, colmask, 0,0))) {
                        SUMA_S_Err("Failed to make copy of master_dset!");
                        exit(1);
                     }
                     SUMA_FreeDset((void*)master_dset); 
                     master_dset = ndset; ndset=NULL;
                     SUMA_free(colmask); colmask=NULL;
                  }
               }
            }

            if (Opt->fwhm > 0 || Opt->tfwhm > 0) {
               if (!Opt->master_name) {/* what is the smoothness of input ? */
                  if (Opt->debug) SUMA_S_Note("Estimating input smoothness");
                  icols = SUMA_FindNumericDataDsetCols(dset, &N_icols);
                  if (N_icols <= 0) { 
                     SUMA_SL_Err("No approriate data columns in dset"); 
                     exit(1); 
                  }
                  if (!(fwhmv = SUMA_estimate_dset_FWHM_1dif(  SO, dset, 
                                                         icols, N_icols,
                                                         Opt->nmask, 
                                                         1, NULL))) {
                     SUMA_S_Err("Rien ne va plus"); exit(1);                                         
                  }
                  SUMA_FWHM_MEAN(fwhmv, N_icols, fwhm_in_start,
                                  Opt->FWHM_mixmode, N);
                  if (N <= 0) {  
                     SUMA_S_Err("Failed to get mean fwhm"); exit(1); 
                  }      
                  if (Opt->debug) 
                     SUMA_S_Notev ( "Have a global FWHM of %f for input %s.\n"
                                 , fwhm_in_start, Opt->in_name);
                  SUMA_free(fwhmv); fwhmv=NULL;
                  SUMA_free(icols); icols=NULL;
               } else {/* Need smoothness of master */
                  if (Opt->oform == SUMA_NO_DSET_FORMAT) Opt->oform = iform;
                  if (Opt->debug) SUMA_S_Note("Estimating master smoothness");
                  icols = SUMA_FindNumericDataDsetCols(master_dset, &N_icols);
                  if (N_icols <= 0) { 
                     SUMA_SL_Err("No approriate data columns in master dset");
                     exit(1); 
                  }
                  if (!(fwhmv = SUMA_estimate_dset_FWHM_1dif(  SO, master_dset, 
                                                         icols, N_icols,
                                                         Opt->nmask, 
                                                         1, NULL))) {
                     SUMA_S_Err("Rien ne va plus"); exit(1);                                         
                  }
                  SUMA_FWHM_MEAN(fwhmv, N_icols, fwhm_master_start,
                                 Opt->FWHM_mixmode, N);
                  if (N <= 0) {  SUMA_S_Err("Failed to get mean fwhm"); exit(1); }      
                  if (Opt->debug) 
                     SUMA_S_Notev ( "Have a global FWHM of %f for master %s.\n"
                                 , fwhm_master_start, Opt->master_name);
                  SUMA_free(fwhmv); fwhmv=NULL;
                  SUMA_free(icols); icols=NULL;
               } 
            }
            
            if (Opt->fwhm > 0) {
               if (fwhm_in_start < 0 && fwhm_master_start < 0) { 
                  SUMA_S_Errv("Flow Error, fwhm_in_start = %f,"
                              "fwhm_master_start= %f.\n"
                              "This should not be.\n",  
                              fwhm_in_start, fwhm_master_start); 
                  exit(1);
               }
               /* blur by a FWHM filter. When users run this, 
                  they expect the FWHMr of the resultant
                  dataset to be something like: 
                  FWHMr = sqrt(FWHMi^2+FWHM^2); where FWHMi is the initial
                  FWHM of the input dataset and FWHM is the additional 
                  blurring they want to apply. */
               if (Opt->master_name) {
                  Opt->tfwhm = sqrt(Opt->fwhm*Opt->fwhm+
                                    fwhm_master_start*fwhm_master_start);
               } else {
                  Opt->tfwhm = sqrt(Opt->fwhm*Opt->fwhm+
                                    fwhm_in_start*fwhm_in_start);
               }
               SUMA_S_Notev ( 
                     "   Have a %s starting FWHM of %.3f, for input dset.\n"
                     "   User requested additional blur by %.3f FWHM\n"
                     "   Resultant %s expected be have %.3f FWHM.\n"
                     , (Opt->master_name ? "master dset":"input dset")
                     , (Opt->master_name ? fwhm_master_start:fwhm_in_start)
                     , Opt->fwhm
                     , (Opt->master_name ? "master dset":"input dset")
                     , Opt->tfwhm);   
               Opt->fwhm = -1; /* at this point, we're blurring to a FWHM */
            }
                       
            if (Opt->tfwhm > 0) { /* blur to a final FWHM */
               if (Opt->N_iter > 0 && Opt->sigma > 0) { 
                  SUMA_S_Err("A useless situation, no need for -target_fwhm");
                  exit(1); 
               }
               
               if (Opt->sigma < 0) {
                  Opt->sigma = SUMA_SigForFWHM( SO->EL->AvgLe, 
                                                Opt->tfwhm, &(Opt->N_iter),
                                                NULL)*SO->EL->AvgLe;  
                  /* if specified, Opt->N_iter is taken into consideration
                   above. But it is not the condition for stopping, only for
                   setting sigma. Opt->N_iter is now of no use and 
                  will be set at the end of the filtering process.  */
                  Opt->N_iter = -1; 
               }               
               
               if (Opt->debug) SUMA_S_Note("Calculating weights");
               SUMA_S_Notev("Smoothing kernel bandwidth (sigma) = %f\n",
                            Opt->sigma);
               wgtd = SUMA_Chung_Smooth_Weights_07(SO, (double)Opt->sigma);
               if (!wgtd) {
                  SUMA_SL_Err("Failed to compute weights.\n");
                  exit(1);
               }

               if (Opt->master_name) {
                  if (Opt->debug) SUMA_S_Note("Smoothing master");
                  if (!SUMA_Chung_Smooth_07_toFWHM_dset (
                           SO, wgtd, 
                           &(Opt->N_iter), &(Opt->tfwhm), 
                           master_dset, Opt->nmask, 
                           1, Opt->FWHM_mixmode, &fwhmhist)){
                     SUMA_S_Err("Failed to blur master data dset");  
                     exit(1);                                     
                  }
                  /* done with master */
                  if (master_dset)  
                     SUMA_FreeDset((void*)master_dset); master_dset=NULL;       
 
                  /* OK, now blur the input data by the previous specs */
                  if (0) {
                     fwhmttt = -1.0;
                     SUMA_LHv("Now blurring the input data by Niter=%d,"
                              " fwhmtt=%f\n",
                              Opt->N_iter, fwhmttt); 
                     if (!SUMA_Chung_Smooth_07_toFWHM_dset (
                              SO, wgtd, 
                              &Opt->N_iter, &fwhmttt, 
                              dset, Opt->nmask, 
                              1, NULL, &fwhmhist)) {
                        SUMA_S_Err("Failed to blur data dset");  
                        exit(1);                                     
                     }
                  } else { 
                     /* maybe faster and slightly more precise than above*/
                     fwhmttt = -1.0;
                     if (Opt->debug) 
                        SUMA_S_Notev("Now blurring the input data by "
                                      "Niter=%d, fwhmtt=%f\n"
                                      "Using SUMA_Chung_Smooth_07_dset"
                                      " (OK if fwhmtt < 0)\n",
                              Opt->N_iter, fwhmttt); 
                     if (!SUMA_Chung_Smooth_07_dset (SO, wgtd, 
                                                     &(Opt->N_iter), &fwhmttt, 
                                                     dset, cs, Opt->nmask, 1)) {
                        SUMA_S_Err("Failed to blur data dset");  
                        exit(1);                                     
                     }
                  }
                  fprintf(SUMA_STDOUT, 
                        "\n"
                        "#Final smoothing parameters via master:\n"
                        "#Niter     Sigma    OutputFWHM\n"
                        " %5d       %.4f     %.3f     \n"
                        "\n", Opt->N_iter , Opt->sigma, fwhmttt);                                      
               } else {
                  if (Opt->debug) 
                        SUMA_S_Notev(
                           "Now blurring the input data by Niter=%d,"
                           " Opt->tfwhm=%f\n"
                           "Using SUMA_Chung_Smooth_07_dset "
                           "(OK if fwhmtt < 0)\n",
                              Opt->N_iter, Opt->tfwhm); 
                  if (!SUMA_Chung_Smooth_07_toFWHM_dset (
                        SO, wgtd, 
                        &(Opt->N_iter), &(Opt->tfwhm), 
                        dset, Opt->nmask, 
                        1, Opt->FWHM_mixmode, &fwhmhist)){
                     SUMA_S_Err("Failed to blur data dset");  
                     exit(1);                                     
                  } 
                  fprintf(SUMA_STDOUT, 
                        "\n"
                        "#Final smoothing parameters from input:\n"
                        "#Niter     Sigma    OutputFWHM\n"
                        " %5d       %.4f     %.3f     \n"
                        "\n", Opt->N_iter , Opt->sigma, Opt->tfwhm);                                      
               }
            } else if (Opt->sigma > 0 && Opt->N_iter > 0) {
               /* just blur like you're told */
               if (Opt->debug) 
                  SUMA_S_Notev(  "Blurring like requested, "
                                 "Niter=%d, sigma=%f\n", 
                                 Opt->N_iter, Opt->sigma);
               if (Opt->debug) 
                  SUMA_S_Note("Calculating weights");
               wgtd = SUMA_Chung_Smooth_Weights_07(SO, (double)Opt->sigma);
               if (!wgtd) {
                  SUMA_SL_Err("Failed to compute weights.\n");
                  exit(1);
               }
               fwhmttt = -1.0;
               if (Opt->debug) 
                  SUMA_S_Note("Blurring");
               if (!SUMA_Chung_Smooth_07_dset (SO, wgtd, 
                                               &(Opt->N_iter), &fwhmttt, 
                                               dset, cs, Opt->nmask, 1)) {
                  SUMA_S_Err("Failed to blur data dset");  
                  exit(1);                                     
               }
               fprintf(SUMA_STDOUT, 
                     "\n"
                     "#Final smoothing parameters as requested:\n"
                     "#Niter    Sigma     OutputFWHM not estimated\n"
                     " %5d      %.4f      %.3f     \n"
                     "\n", Opt->N_iter , Opt->sigma, -1.0);      
            }
            
            if (fwhmhist) {
               if (Opt->debug) {
                  SUMA_S_Notev("Writing smoothing record to prefix %s\n",
                              Opt->out_name);
               }
               SUMA_WriteSmoothingRecord (SO, 
                                          fwhmhist, Opt->N_iter, 
                                          &(Opt->sigma), 1,
                                          Opt->out_name);
            }
            
            if (Opt->detrend_in > 0 || Opt->detpoly_in > -1) {
               if (Opt->debug)SUMA_S_Note("Retrending input");
               /* need to retrend result */
               if (!(inset = SUMA_sumadset2afnidset(&dset, 1, 1))) {
                  SUMA_S_Err("Failed to transform surface dset to afni dset");
                  exit(1);
               }
               
               if (!(THD_retrend_dataset( inset , nref_in , 
                                          ref_in , Opt->scaleinput , 
                                          Opt->nmask , corder_inar ))) { 
                  SUMA_S_Err("retrending failed!") ;
                  exit(1);
               }
               
               for(jj=0;jj<nref_in;jj++) free(ref_in[jj]) ;
               free(ref_in);

               /* Now back to SUMA_DSETand kill afni volume*/
               dset = SUMA_afnidset2sumadset(&inset, 1, 1); 
            }
            if (wgtd) { /* this was allocate without allocate2D
                       You can't use free2D to freeit because
                       of the use of allocation tracking
                       which is disabled in allocate2D 
                           SUMA_free2D((char **)wgtd,SO->N_Node);  */
               for (jj=0;jj<SO->N_Node;++jj)
                  if (wgtd[jj]) SUMA_free(wgtd[jj]);
               SUMA_free(wgtd);
               wgtd = NULL;
            }
         }
         break; 
      
      case SUMA_HEAT_05_Pre_07:
         /* Moo Chung's method for interpolation weights */
         {
            double avg_wt, sequiv;
            
            if (Opt->N_iter < 0 &&  Opt->fwhm > 0 && Opt->sigma < 0)  {
               /* make a suggestion */
               Opt->sigma = sqrt(-SO->EL->AvgLe/(2*log(0.01))); /* making the average SUMA_CHUNG_KERNEL_NUMER be 1 percent */
               /* have sigma and fwhm, what is N? */
               sequiv  = Opt->fwhm * 0.42466090;
               Opt->N_iter = SUMA_POW2(sequiv/Opt->sigma);
               if (Opt->N_iter % 2) ++Opt->N_iter;
               if (Opt->N_iter < 2) Opt->N_iter = 2; /* need a few iterations */
               /* now reset sigma based on number of iterations */
               sequiv = Opt->fwhm * 0.42466090;
               Opt->sigma = sequiv / sqrt(Opt->N_iter);
            } else if (Opt->N_iter < 0) {
               SUMA_S_Errv("Negative number of iterations (%d).Should not be here.\n", Opt->N_iter);
               exit(1);
            }
         
            fprintf (SUMA_STDERR,"Effective FWHM = %f, kernel bandwidth = %f, N_iter = %d\n", Opt->fwhm, Opt->sigma, Opt->N_iter);

            /* check that sigma is not too small or too big relative to average segment length */
            avg_wt = SUMA_CHUNG_KERNEL_NUMER(SO->EL->AvgLe,Opt->sigma);
            fprintf(SUMA_STDERR, "Kernel Bandwidth / Average Edge Distance = %f/%f = %f\n"
                                 "   Corresponding Kernel Numerator = %g\n", 
                                    Opt->sigma, SO->EL->AvgLe,  Opt->sigma/SO->EL->AvgLe, avg_wt);
            if (avg_wt > 0.10) {
               SUMA_S_Warnv("Average weight assigned per node is of %g\n"
                            "You can increase the number of\n"
                            "iterations.\n", avg_wt);
            }
            if (avg_wt < 1e-4) {
               SUMA_S_Warnv("Average weight assigned per node is of %g\n"
                            "It is advisable to decrease the number of\n"
                            "iterations, if possible.\n", avg_wt);
            }
            if (avg_wt < 1e-7) {
               SUMA_S_Warnv("Average weight assigned per node is of %g\n"
                            "Cannot trust results. Reduce number of\n"
                            "iterations, if possible.\n", avg_wt);
            }
            
            /* now load the input data */
            iform = SUMA_GuessFormatFromExtension(Opt->in_name, NULL);
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
            wgt = SUMA_Chung_Smooth_Weights_05_Pre_07(SO, Opt->sigma);
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
            
            if (Opt->N_iter <= 0) {
               SUMA_S_Errv("Negative Niter (%d)\n", Opt->N_iter);
               exit(1);
            }
            if (!SUMA_Chung_Smooth_05_Pre_07_dset ( SO, wgt, 
                                          (Opt->N_iter), (Opt->fwhm), 
                                          dset, cs, Opt->nmask, Opt->strict_mask)) {
               SUMA_S_Err("Failed in  SUMA_Chung_Smooth_05_Pre_07_dset");
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
      
      case SUMA_BRUTE_FORCE:
         {
            /* now load the input data */
            iform = SUMA_GuessFormatFromExtension(Opt->in_name, NULL);
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
                                    

            if (LocalHead) {
               SUMA_LHv("Input %s:\n", Opt->in_name);
               SUMA_ShowDset(dset, 0, NULL);
            }
            
            if (!SUMA_Offset_Smooth_dset( SO, Opt->fwhm, Opt->OffsetLim, Opt->N_iter, dset, cs, Opt->nmask, Opt->strict_mask)) {
               SUMA_S_Err("Failed in SUMA_Offset_Smooth_dset ");
               exit(1);  
            }

            if (LocalHead) {
               etime_GetOffset = SUMA_etime(&start_time,1);
               fprintf(SUMA_STDERR, "%s: Total processing took %f seconds for %d nodes.\n"
                                 "Projected time per 100000 nodes is: %f minutes\n", 
                                       FuncName, etime_GetOffset, SO->N_Node, 
                                       etime_GetOffset * 100000 / 60.0 / (SO->N_Node));
            }

            if (0 && LocalHead) {
               SUMA_LH("See dsmooth.1D");
               SUMA_disp_vecmat (dsmooth, SO->N_Node, 3, 1,  d_order, NULL, YUP);
            }
            /* writing of results is done below */
            etime_GetOffset_all = SUMA_etime(&start_time_all,1);
            fprintf(SUMA_STDERR, "%s: Done.\nSearch to %f mm took %f minutes for %d nodes.\n" , 
                                 FuncName, Opt->lim, etime_GetOffset_all / 60.0 , SO->N_Node);

         }
         break;
      
      case SUMA_NN_GEOM:
         /* brute forcem nearset neighbor interpolation */
         {
            if (LocalHead) SUMA_etime(&start_time,0);
            
            d_order =  SUMA_ROW_MAJOR; 
            
            dsmooth = SUMA_NN_GeomSmooth( SO, Opt->N_iter, SO->NodeList,
                                          3, d_order, NULL, cs, Opt->nmask,
                                          Opt->strict_mask);
            if (0 && LocalHead) {
               SUMA_LH("See dsmooth.1D");
               SUMA_disp_vecmat (dsmooth, 
                                 SO->N_Node, 3, 1,  
                                 d_order, NULL, YUP);
            }
            if (!dsmooth) {
               SUMA_SL_Err("Failed in SUMA_NN_Geom_Smooth");
               exit(1);
            }
            
            /* writing of results is done below */
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
               /* fprintf(stderr,"%d, %d\n", SUMA_Get_Taubin_Weights() ,
                            SUMA_EQUAL); */
               SUMA_SL_Err("Weights improperly initialized!");
               exit(1);
            }
             
            dsmooth = SUMA_Taubin_Smooth (SO, wgt, 
                      Opt->l, Opt->m, SO->NodeList, 
                      Opt->N_iter, 3, d_order,
                      NULL, cs, Opt->nmask, Opt->strict_mask); 

            if (LocalHead) {
               etime_GetOffset = SUMA_etime(&start_time,1);
               fprintf(SUMA_STDERR, 
                  "%s: Total processing took %f seconds for %d nodes.\n"
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
   
   
   if (Opt->Method == SUMA_NN_GEOM)
   {
      if (Opt->MatchMethod) {   
         SUMA_LH("Fixing shrinkage...");
         /* Create a surface that is a descendant of SO, 
            use the new coordinates */
         SOnew = SUMA_CreateChildSO( SO,
                                     dsmooth, SO->N_Node,
                                     NULL, -1,
                                     0); /* SOnew->NodeList is now == dsmooth */

         /* fix the shrinking ..*/

         switch (Opt->MatchMethod) {
            case 1:
               if (!SUMA_EquateSurfaceSize(SOnew, SO, Opt->lim, cs)) {
                  SUMA_SL_Warn(  "Failed to fix surface size.\n"
                                 "Trying to finish ...");
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
         fprintf(SUMA_STDERR, 
                  "%s: Total processing took %f seconds for %d nodes.\n"
                  "Projected time per 100000 nodes is: %f minutes\n", 
                     FuncName, etime_GetOffset, SO->N_Node, 
                     etime_GetOffset * 100000 / 60.0 / (SO->N_Node));
      }

   }
   /* write out the filtered geometry. 
      Should not be executed for data smoothing */
   if (Opt->surf_out) {
      SUMA_SO_File_Type ft=SUMA_FT_NOT_SPECIFIED;
      SUMA_SO_File_Type ff=SUMA_FF_NOT_SPECIFIED;
      if (!dsmooth) {
         SUMA_SL_Err("NULL dsmooth for geometry smoothing.\n"
                     "Either failed to smooth or logical error.");
         exit(1);
      }
      if ((ft = SUMA_guess_surftype_argv(Opt->surf_out)) <= 
          SUMA_FT_NOT_SPECIFIED) ft = SO->FileType;
      SUMA_free(SO->NodeList); 
      SO->NodeList = dsmooth; dsmooth = NULL;  /* replace NodeList */
      if (!(SUMA_Save_Surface_Object_Wrap ( Opt->surf_out, NULL,
                                            SO, ft, ff, NULL))) {
         fprintf (SUMA_STDERR,
                  "Error %s: Failed to write surface object.\n", FuncName);
         exit (1);
      } 
   } else {
      if (  Opt->Method != SUMA_LB_FEM && 
            Opt->Method != SUMA_HEAT_05_Pre_07 && Opt->Method != SUMA_HEAT_07 && 
            Opt->Method != SUMA_BRUTE_FORCE) {
         if (!dsmooth) {
            SUMA_SL_Err("NULL dsmooth for data smoothing. "
                        "Either failed to smooth or logical error.");
            exit(1);
         }      
         fileout = fopen(Opt->out_name, "w");
         if (Opt->AddIndex) SUMA_disp_vecmat (dsmooth, SO->N_Node, ncol, 1, 
                                              d_order, fileout, YUP);
         else SUMA_disp_vecmat (dsmooth, SO->N_Node, ncol, 1, 
                                d_order, fileout, NOPE);
         fclose(fileout); fileout = NULL;
      } else if ( Opt->Method == SUMA_LB_FEM || 
                  Opt->Method == SUMA_HEAT_05_Pre_07 || 
                  Opt->Method == SUMA_HEAT_07 || 
                  Opt->Method == SUMA_BRUTE_FORCE) {
         SUMA_NEWDSET_ID_LABEL_HIST(dset, Opt->out_name) ;
         if (Opt->AddIndex) SUMA_SetAddIndex_1D(1);
         /* Add the history line */
         if (!SUMA_AddNgrHist (dset->ngr, FuncName, argc, argv)) {
            SUMA_SL_Err("Failed in SUMA_AddNgrHist");
         }
         SUMA_LHv("About to write output dset %s, oform %d\n", 
                  Opt->out_name, Opt->oform);
         ooo = SUMA_WriteDset_s( Opt->out_name, dset, Opt->oform, 
                                 Opt->overwrite, 0);
         SUMA_FreeDset(dset); dset = NULL; SUMA_free(ooo); ooo=NULL;
      } else {
         SUMA_S_Err("Fix me");
      }
   }



   /* you don't want to exit rapidly because SUMA might not be done processing the last elements*/
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
   mri_free(im); im = NULL;   /* done with that baby */
   if (!SUMA_FreeSpecFields(Spec)) {
      SUMA_S_Err("Failed to free Spec fields");
   } SUMA_free(Spec); Spec = NULL;
   if (cs) cs = NULL; /* ps->cs if freed below */
   if (SF_name) SUMA_free(SF_name);
   if (SO) SUMA_Free_Surface_Object(SO); 
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

