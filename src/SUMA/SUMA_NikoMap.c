/*USE This sample to start writing standalone programs.
Change NikoMap to the program name of your choosing.
*/
#include "SUMA_suma.h"

SUMA_SurfaceViewer *SUMAg_cSV = NULL; /*!< Global pointer to current Surface Viewer structure*/
SUMA_SurfaceViewer *SUMAg_SVv = NULL; /*!< Global pointer to the vector containing the various Surface Viewer Structures 
                                    SUMAg_SVv contains SUMA_MAX_SURF_VIEWERS structures */
int SUMAg_N_SVv = 0; /*!< Number of SVs realized by X */
SUMA_DO *SUMAg_DOv = NULL;   /*!< Global pointer to Displayable Object structure vector*/
int SUMAg_N_DOv = 0; /*!< Number of DOs stored in DOv */
SUMA_CommonFields *SUMAg_CF = NULL; /*!< Global pointer to structure containing info common to all viewers */

void usage_NikoMap (SUMA_GENERIC_ARGV_PARSE *ps)
{
      static char FuncName[]={"usage_NikoMap"};
      char * s = NULL, *sio=NULL, *st = NULL, *sts = NULL;
      int i;
      s = SUMA_help_basics();
      sio  = SUMA_help_IO_Args(ps);
      printf ( "\n"
               "Usage: NikoMap  <-i_TYPE SURFACE> <-prefix PREFIX>\n"
               "                <-grid_parent GRID_VOL> [-sv SURF_VOL] \n"
               " \n"
               "  Mandatory Parameters:\n"
               "     -i_TYPE SURFACE: Specify input surface.\n"
               "             You can also use -t* and -spec and -surf\n"
               "             methods to input surfaces. See below\n"
               "             for more details.\n"
               "     -prefix PREFIX: Prefix of output datasets.\n"
               "     -grid_parent GRID_VOL: Specifies the grid for the\n"
               "                  FMRI data volume."
               "   Patch options:\n"
               "     -patch_grow: Create surface patch growing data.\n"
               "     -grow_dist D: Grow patch up to Dmm away from each node.\n"
               "                   Distance is the shortest along the edges of \n"
               "                    the surface.\n"
               "    *** With these options you will get a file called:\n"
               "     PREFIX_pg.1D.dset with each row containing:\n"
               "     <node index (n)> <number of neighbors of n (Kn)> followed by\n"
               "     Kn sets of: \n"
               "     <neighbor node index (m)> <distance of m to n> \n"
               "     <X Y Z of estimated propagation location of m>\n"
               "     The last three parameters represent the estimated\n"
               "     location of m in the next contour (layer) of neighbors\n"
               "     of n\n"
               "\n"
               "   Volume-->Surface options:\n"
               "     -closest_node: Create a file that outputs the closest\n"
               "                    node for each voxel in GRID_VOL.\n"
               "    *** With this option you'll get a file called:\n"
               "     PREFIX_cn.1D.dset with each row containing:\n"
               "     <Voxel 1D index (v)> <I J K of v> <closest node n> <distance of n to v>\n"
               "\n"
               "   Optional Options:\n"
               "   -debug BUG: Debug level\n"
               "   -node_debug NODE_DBG: Output lots of info for a particular node.\n"
               "                         This option also produces a file called\n"
               "                         PREFIX_pd_dbg_node_NODE_DBG.1D.dset\n"
               "\n"
               "   Sample command:\n"
               "   NikoMap -i_ply rs_acpc_tal_LH_GM_half.ply      \\\n"
               "           -sv rs_acpc_tal+tlrc. -prefix output   \\\n"
               "           -grid_parent coarse+tlrc.              \\\n"
               "           -closest_node                          \\\n"
               "           -patch_grow -grow_dist 7.5             \\\n"
               "           -debug 3 -node_debug 4                 \\\n"
               " \n"
               "%s"
               "%s"
               "\n", sio,  s);
      SUMA_free(s); s = NULL; SUMA_free(st); st = NULL; SUMA_free(sio); sio = NULL;       
      s = SUMA_New_Additions(0, 1); printf("%s\n", s);SUMA_free(s); s = NULL;
      printf("       Ziad S. Saad SSCC/NIMH/NIH saadz@mail.nih.gov     \n");
      exit(0);
}

#define CLOSEST_NODE (1<<0)
#define PATCH_GROW   (1<<1)

SUMA_GENERIC_PROG_OPTIONS_STRUCT *SUMA_NikoMap_ParseInput(char *argv[], int argc, SUMA_GENERIC_ARGV_PARSE *ps)
{
   static char FuncName[]={"SUMA_NikoMap_ParseInput"}; 
   SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt=NULL;
   int kar;
   SUMA_Boolean brk;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   Opt = SUMA_Alloc_Generic_Prog_Options_Struct();
   Opt->iopt = 0;
   Opt->r = 10.0;
   Opt->NodeDbg = -1;
   kar = 1;
   brk = NOPE;
	while (kar < argc) { /* loop accross command ine options */
		/*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
		if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
			 usage_NikoMap(ps);
          exit (0);
		}
		
		SUMA_SKIP_COMMON_OPTIONS(brk, kar);
      
      if (!brk && (strcmp(argv[kar], "-prefix") == 0))
      {
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need a number after -prefix \n");
            exit (1);
         }
         Opt->out_vol_prefix = SUMA_AfniPrefix(argv[++kar], Opt->out_vol_view, NULL, &(Opt->out_vol_exists));
         
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-grid_parent") == 0))
      {
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need a dataset after -grid_parent \n");
            exit (1);
         }
         Opt->out_grid_prefix = SUMA_AfniPrefix(argv[++kar], Opt->out_grid_view, NULL, &(Opt->out_grid_exists));
         if (!SUMA_AfniExistsView(Opt->out_grid_exists, Opt->out_grid_view)) {
            fprintf(SUMA_STDERR, "Error 3dSurfMask:\nGrid parent %s%s does not exist.\n", Opt->out_grid_prefix, Opt->out_grid_view);
            exit(1);
         }
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-closest_node") == 0))
      {
         Opt->iopt = Opt->iopt | CLOSEST_NODE;
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-patch_grow") == 0))
      {
         Opt->iopt = Opt->iopt | PATCH_GROW;
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-grow_dist") == 0))
      {
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need a value after -grow_dist \n");
            exit (1);
         }
         Opt->r = atof(argv[++kar]);
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-node_debug") == 0))
      {
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need a value after -node_debug \n");
            exit (1);
         }
         Opt->NodeDbg = atoi(argv[++kar]);
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-debug") == 0))
      {
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need a number after -debug \n");
            exit (1);
         }
         
         Opt->debug = atoi(argv[++kar]);
         brk = YUP;
      }
      
      if (!brk && !ps->arg_checked[kar]) {
			fprintf (SUMA_STDERR,"Error NikoMap:\nOption %s not understood. Try -help for usage\n", argv[kar]);
			exit (1);
		} else {	
			brk = NOPE;
			kar ++;
		}
   }
   
   SUMA_RETURN(Opt);
}


int main (int argc,char *argv[])
{/* Main */    
   static char FuncName[]={"NikoMap"}; 
   SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt;  
   SUMA_GENERIC_ARGV_PARSE *ps=NULL;
   SUMA_SurfSpecFile *Spec = NULL;
   int *isin=NULL;
   int  i = -1, ii, jj, kk, il, N_Spec=0;
   SUMA_FORM_AFNI_DSET_STRUCT *OptDs = NULL;
   SUMA_SurfaceObject *SO = NULL;
   SUMA_VOLPAR *vp = NULL;
   THD_3dim_dataset *dset = NULL;
   char *vpname=NULL;
   int *closest_node = NULL;
   float *closest_dist = NULL;
   SUMA_OFFSET_STRUCT *OffS_out=NULL;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_STANDALONE_INIT;
	SUMA_mainENTRY;

   /* Allocate space for DO structure */
	SUMAg_DOv = SUMA_Alloc_DisplayObject_Struct (SUMA_MAX_DISPLAYABLE_OBJECTS);
   ps = SUMA_Parse_IO_Args(argc, argv, "-i;-t;-spec;-sv;-o;");
   
   if (argc < 2) {
      usage_NikoMap(ps);
      exit (1);
   }
   
   Opt = SUMA_NikoMap_ParseInput (argv, argc, ps);

   if (Opt->debug > 2) LocalHead = YUP;
   
   /* check on inputs */
   if (ps->s_N_surfnames + ps->i_N_surfnames + ps->t_N_surfnames != 1) {
      SUMA_S_Err("Multiple surface specifications used. Only one surface allowed.");
      exit(1);
   }

   Spec = SUMA_IO_args_2_spec(ps, &N_Spec);
   if (N_Spec == 0) {
      SUMA_S_Err("No surfaces found.");
      exit(1);
   }
   if (N_Spec != 1) {
      SUMA_S_Err("Multiple spec at input.");
      exit(1);
   }

   /* read in one surface for now */
   SO = SUMA_Load_Spec_Surf(Spec, 0, ps->sv[0], 0);
   if (!SO) {
         fprintf (SUMA_STDERR,"Error %s:\n"
                              "Failed to find surface\n"
                              "in spec file. \n",
                              FuncName );
         exit(1);
      
   }   
   
   if (Opt->out_grid_prefix) {
      vpname = SUMA_append_string(Opt->out_grid_prefix, Opt->out_grid_view);
      vp = SUMA_VolPar_Attr(vpname);
   } else { 
      vp = SO->VolPar;
      if (!vp) {
         fprintf (SUMA_STDERR,"Error %s:\n"
                                 "Need a grid parent.\n",
                                 FuncName );
            exit(1);
      }
      vpname = SUMA_copy_string(ps->sv[0]);
      if (!SUMA_AfniView(ps->sv[0], Opt->out_grid_view)) {
         fprintf (SUMA_STDERR,"Error %s:\n"
                                 "Failed to get view!!!\n",
                                 FuncName );
            exit(1);
         
      }
   }
   
   if (LocalHead) {
      fprintf(SUMA_STDERR,"%s: Using %s for grid\n", FuncName, vpname);
   }
   
   /* some checks ...*/
   if (!Opt->out_vol_prefix) { 
      Opt->out_vol_prefix = SUMA_AfniPrefix("3dSurfMask", NULL, NULL, &(Opt->out_vol_exists)); 
      strncpy(Opt->out_vol_view, Opt->out_grid_view, SUMA_VIEW_LENGTH);
   }
   
   if (SUMA_AfniExistsView(Opt->out_vol_exists, Opt->out_vol_view)) {
      fprintf(SUMA_STDERR, "Error 3dSurfMask:\nOutput volume %s%s exists.\n", Opt->out_vol_prefix, Opt->out_vol_view);
      exit(1);
   }
   
   if (Opt->out_grid_prefix) {
      if (!SUMA_AfniExistsView(Opt->out_grid_exists, Opt->out_grid_view)) {
         fprintf(SUMA_STDERR, "Error 3dSurfMask:\nGrid parent %s%s does not exist.\n", Opt->out_grid_prefix, Opt->out_grid_view);
         exit(1);
      }
   }
   
   if (Opt->iopt & CLOSEST_NODE) {
      SUMA_LH("Doing closest node");
      /* closest node to each voxel in volume */
      closest_node = (int *)SUMA_malloc(sizeof(int)*vp->nx*vp->ny*vp->nz);
      closest_dist = (float *)SUMA_malloc(sizeof(float)*vp->nx*vp->ny*vp->nz);

      if (!closest_node || !closest_dist) {
         SUMA_SL_Err("Failed to allocate");
         exit(1);
      }
      if (!SUMA_ClosestNodeToVoxels(SO, vp, closest_node, closest_dist, NULL, 1)) {
         SUMA_SL_Err("Failed to find closest nodes to voxels");
         exit(1);
      }

      /*----- output volume dset */
      OptDs = SUMA_New_FormAfniDset_Opt();
      if (Opt->out_vol_prefix) {
         SUMA_FileName NewName = SUMA_StripPath(Opt->out_vol_prefix);
         OptDs->prefix = SUMA_append_string(NewName.FileName,"_cn"); SUMA_free(NewName.FileName); NewName.FileName = NULL;
         OptDs->prefix_path = NewName.Path; NewName.Path = NULL;
      }  else {
         OptDs->prefix = SUMA_copy_string("NikoMap_cn");
         OptDs->prefix_path = SUMA_copy_string("./");
      }

      /* write dset */
      OptDs->master = SUMA_copy_string(vpname);
      OptDs->datum = MRI_float;
      OptDs->full_list = 1;
      { 
         dset = SUMA_FormAfnidset (NULL, closest_dist, vp->nx*vp->ny*vp->nz, OptDs);
         if (!dset) {
            SUMA_SL_Err("Failed to create output dataset!");
         } else {
            tross_Make_History( FuncName , argc,argv , dset ) ;
            DSET_write(dset) ;
         }
         if (dset) { DSET_delete(dset); dset = NULL; }
      }

      /*----- output files in table form*/
      { 
         char *stmp = NULL, *nameout = NULL, *histnote=NULL;
         FILE *fout=NULL;
         nameout = SUMA_append_string(Opt->out_vol_prefix, "_cn.1D.dset");
         fout = fopen(nameout,"w");
         if (!fout) {
            SUMA_S_Err("Failed to open file for output");
            exit(1);
         }
         histnote = SUMA_HistString (NULL, argc, argv, NULL);
         fprintf(fout,  "#Col. 0 Voxel index (1D)\n"
                        "#Col. 1..4 Voxel i,j,k index\n"
                        "#Col. 5 Closest node\n"
                        "#Col. 6 Closest node's distance\n"
                        "#History:%s\n", histnote);
         for (i=0; i<vp->nx*vp->ny*vp->nz; ++i) {
            SUMA_1D_2_3D_index(i, ii, jj, kk, vp->nx, vp->nx*vp->ny);
            fprintf(fout, "%8d   %4d %4d %4d   %8d   %4.5f\n", 
               i, ii, jj, kk, closest_node[i], closest_dist[i]);
         }
         fclose(fout); fout = NULL;
         if (stmp) SUMA_free(stmp); stmp = NULL;
         if (nameout) SUMA_free(nameout); nameout = NULL;
         if (histnote) SUMA_free(histnote); histnote = NULL;
      }   
   }
   
   if (Opt->iopt && PATCH_GROW) {
      int cnt = 0;
      char *stmp = NULL, *nameout = NULL, *histnote=NULL, sbuf[500], OptS[]={"DoProp"};
      FILE *fout=NULL, *fout_dbg=NULL;
      nameout = SUMA_append_string(Opt->out_vol_prefix, "_pg.1D.dset");
      fout = fopen(nameout,"w");
      if (!fout) {
         SUMA_S_Err("Failed to open file for output");
         exit(1);
      }
      histnote = SUMA_HistString (NULL, argc, argv, NULL);
      if (SUMA_iswordin(OptS,"DoProp") == 1) {
         fprintf(fout,  "#Col. 0 Node n's index \n"
                        "#Col. 1 Number of neighbors listed for node n \n"
                        "#Col. 2+5*i ith neighbor of Node n (i=0..)\n"
                        "#Col. 3+5*i distance of ith neighbor from Node n\n"
                        "#Col. 4..6+5*i Propagation location of ith neighbor of Node n\n"
                        "#History:%s\n", histnote);
      } else {
         fprintf(fout,  "#Col. 0 Node n's index \n"
                        "#Col. 1 Number of neighbors listed for node n \n"
                        "#Col. 2+2*i ith neighbor of Node n (i=0..)\n"
                        "#Col. 3+2*i distance of ith neighbor from Node n\n"
                        "#History:%s\n", histnote);
      }
      if (Opt->NodeDbg >= 0) {
         sprintf(sbuf,"_pg_dbg_node_%d.1D.dset", Opt->NodeDbg);
         stmp = SUMA_append_string(Opt->out_vol_prefix, sbuf);
         fout_dbg = fopen(stmp,"w");
         if (!fout_dbg) {
            SUMA_S_Err("Failed to open file for output");
            exit(1);
         }
         if (Opt->NodeDbg >= 0) {
            fprintf(fout_dbg,
                     "#The first row is special, it is used to indicate\n"
                     "#the index of node n (%d) about which the patch is grown.\n"
                     "#Col. 0 Node m's index (m is the node neighboring n)\n"
                     "#Col. 1 Graph distance (along surface edges) from n to m \n"
                     "#Col. 2..4 Propagation location of node m\n"
                     "#          this is the projected location of node m\n"
                     "#          if the contour was stretched to the next \n"
                     "#          neighborhood layer.\n"
                     "#History:%s\n", Opt->NodeDbg, histnote);
            fprintf(fout_dbg,"%8d 0.0 -1.0 -1.0 -1.0\n", Opt->NodeDbg);  
         } else {
            fprintf(fout_dbg,  
                     "#The first row is special, it is used to indicate\n"
                     "#the index of node n (%d) about which the patch is grown.\n"
                     "#Col. 0 Node m's index (m is the node neighboring n)\n"
                     "#Col. 1 Graph distance (along surface edges) from n to m \n"
                     "#History:%s\n", Opt->NodeDbg, histnote);
            fprintf(fout_dbg,"%8d 0.0\n", Opt->NodeDbg);
         }
      }
      
      if (!SO->FN) {
         if (!SUMA_SurfaceMetrics_eng(SO, "EdgeList", NULL, 0,  SUMAg_CF->DsetList)) {
            SUMA_S_Err("Failed to create EdgeList");
            exit(1);
         }   
      }
      
      SUMA_LH( "Patch Grow, the memory expensive way\n"
               "Calculating OffS_out ... Very SLOWWWW...");
      if (Opt->NodeDbg >= 0) SUMA_Set_OffsetDebugNode(Opt->NodeDbg);
      OffS_out = SUMA_FormNeighbOffset (SO, Opt->r, OptS, NULL, -1.0);
      SUMA_LH("Writing OffS_out ... ");
      for (i=0; i < SO->N_Node; ++i) {
         fprintf(fout,"%d %d ", i, OffS_out[i].N_Neighb); /* node index */
         if (OffS_out[i].Neighb_PropLoc) {
            for (il=0; il<OffS_out[i].N_Neighb; ++il) {
               if (OffS_out[i].Neighb_dist[il] <= Opt->r) {
                  fprintf(fout,"%d %.2f %.2f %.2f %.2f ",
                     OffS_out[i].Neighb_ind[il], OffS_out[i].Neighb_dist[il],
                        OffS_out[i].Neighb_PropLoc[3*il], 
                        OffS_out[i].Neighb_PropLoc[3*il+1], 
                        OffS_out[i].Neighb_PropLoc[3*il+2]);
               }
            }
         } else {
            for (il=0; il<OffS_out[i].N_Neighb; ++il) {
               if (OffS_out[i].Neighb_dist[il] <= Opt->r) {
                  fprintf(fout,"%d %.2f",
                     OffS_out[i].Neighb_ind[il], OffS_out[i].Neighb_dist[il]);
               }
            }
         }
         fprintf(fout,"\n");
         if (Opt->NodeDbg == i) {
            if (OffS_out[i].Neighb_PropLoc) {
               for (il=0; il<OffS_out[i].N_Neighb; ++il) {
                  fprintf(fout_dbg,"%8d   %3.3f   %4.2f   %4.2f   %4.2f   \n",
                     OffS_out[i].Neighb_ind[il], OffS_out[i].Neighb_dist[il],
                     OffS_out[i].Neighb_PropLoc[3*il], 
                     OffS_out[i].Neighb_PropLoc[3*il+1], 
                     OffS_out[i].Neighb_PropLoc[3*il+2]);
               }
            } else {
               for (il=0; il<OffS_out[i].N_Neighb; ++il) {
                  fprintf(fout_dbg,"%8d   %3.3f   \n",
                     OffS_out[i].Neighb_ind[il], OffS_out[i].Neighb_dist[il]);
               }
            }
         }
         if (Opt->debug) {
            ++cnt;
            if (!(cnt % 1000)) {
               fprintf(SUMA_STDERR,". @ %8d   (%3.2f%%)\n", 
                        i, (float)cnt/(float)(SO->N_Node)*100.0); fflush(SUMA_STDERR);
            }
         }
      }
      
      if (fout) fclose(fout); fout = NULL;
      if (fout_dbg) fclose(fout_dbg); fout_dbg = NULL;
      if (stmp) SUMA_free(stmp); stmp = NULL;
      if (nameout) SUMA_free(nameout); nameout = NULL;
      if (histnote) SUMA_free(histnote); histnote = NULL;
      OffS_out = SUMA_free_NeighbOffset(SO, OffS_out);
   }
         
   /* Frenching */   
   if (closest_node) SUMA_free(closest_node); closest_node = NULL;
   if (closest_dist) SUMA_free(closest_dist); closest_dist = NULL;  
   if (vpname) SUMA_free(vpname); vpname = NULL;
   if (OptDs) { OptDs->mset = NULL; OptDs = SUMA_Free_FormAfniDset_Opt(OptDs);  }
   if (vp != SO->VolPar) SUMA_Free_VolPar(vp); vp = NULL;
   if (SO) SUMA_Free_Surface_Object(SO); SO = NULL;
   if (ps) SUMA_FreeGenericArgParse(ps); ps = NULL;
   if (N_Spec) {
      int k=0; 
      for (k=0; k<N_Spec; ++k) {
         if (!SUMA_FreeSpecFields(&(Spec[k]))) { SUMA_S_Err("Failed to free spec fields"); } 
      }
      SUMA_free(Spec); Spec = NULL; N_Spec = 0;
   }
   if (Opt) Opt = SUMA_Free_Generic_Prog_Options_Struct(Opt);
   if (!SUMA_Free_CommonFields(SUMAg_CF)) SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);
   exit(0);   
} 
