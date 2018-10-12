
#define VERSION "version 1.11 (October 6, 2004)"

/*----------------------------------------------------------------------
 * SurfMeasures - compute measures from the surface dataset(s)
 *
 * This program takes as input one or two surfaces (as part of a
 * spec file), and outputs a 1D format surface dataset, containing
 * requested measures from the input surfaces.
 *
 * Valid measures (to be supplied via the -func option) are:
 *
 *     ang_norms	: angle between the 2 surface normals
 *     ang_ns_A		: angle between the segment and first normal
 *     ang_ns_B		: angle between the segment and second normal
 *     coord_A		: first xyz coordinate
 *     coord_B		: second xyz coordinate
 *     n_area_A		: area for each node on the first surface
 *     n_area_B		: area for each node on the second surface
 *     n_avearea_A	: average area of trianges for each node on surface
 *     n_avearea_B	: average area of trianges for each node on surface
 *     n_ntri		: number of included trianges for each node
 *     nodes            : node index
 *     node_vol		: between surface volume per node
 *     node_volg		: between surface volume per node estimated with 
 *                     Gauss' theorem
 *     norm_A		: vector of normal at node on first surface
 *     norm_B		: vector of normal at node on second surface
 *     thick            : thickness - length of node segment
 *
 * Final info options (accessed via '-info_XXXX'):
 *
 *     all              : combines all '-info_XXXX' options
 *     area             : display total area of each surface
 *     norms            : display norm averages
 *     thick            : display min/max thickness
 *     vol              : display total volume and areas
 *     volg             : display total volume and areas estimated with 
 *                        Gauss' theorem
 *
 * See "SurfMeasures -help" for more information.
 *
 * Author: R. Reynolds
 *----------------------------------------------------------------------
*/

/* use history as a global string to print out with '-hist' option */

static char g_history[] =
    "----------------------------------------------------------------------\n"
    "history :\n"
    "\n"
    "1.0 December 01, 2003  [rickr]\n"
    "  - initial release\n"
    "\n"
    "1.1 December 02, 2003  [rickr]\n"
    "  - fixed stupid macro error, grrrr...\n"
    "\n"
    "1.2 December 03, 2003  [rickr]\n"
    "  - added '-cmask' and '-nodes_1D' options\n"
    "\n"
    "1.3 December 11, 2003  [rickr]\n"
    "  - added required program argument(s): '-surf_A' (and 'B' for 2 surfs)\n"
    "      o  see '-help' for a description and examples\n"
    "  - added SUMA_spec_select_surfs() and SUMA_spec_set_map_refs() so that:\n"
    "      o  only requested surfaces are loaded\n"
    "      o  spec files no longer need 'SAME' as MappingRef\n"
    "  - fixed loss of default node indices (from nodes_1D change)\n"
    "  - added '-hist' option\n"
    "  - display angle averages only if at least 1 total is computed\n"
    "\n"
    "1.4 January 22, 2004  [rickr]\n"
    "  - fixed error with '-nodes_1D' indexing (fp0 in write_output())\n"
    "    (for output of node coordinates)\n"
    "  - added '-sv' option to examples\n"
    "  - reversed history list (most recent last) for '-hist' option\n"
    "\n"
    "1.5 January 23, 2004  [rickr]\n"
    "  - SUMA_isINHmappable() is deprecated, check with AnatCorrect field\n"
    "\n"
    "1.6 February 11, 2004  [rickr]\n"
    "  - add a little debug help for !AnatCorrect case\n"
    "\n"
    "1.7 February 23, 2004  [rickr]\n"
    "  - added functions 'n_avearea_A', 'n_avearea_B', 'n_ntri'\n"
    "\n"
    "1.8 March 26, 2004  [ziad]\n"
    "  - DsetList added to SUMA_LoadSpec_eng() and SUMA_SurfaceMetrics_eng()\n"
    "\n"
    "1.9 July 29, 2004  [rickr]\n"
    "  - Remove check for anat correct.\n"
    "\n"
    "1.10 August 11, 2004  [rickr]\n"
    "  - Add comment about volume being too large.\n"
    "\n"
    "1.11 October 12, 2004  [rickr]\n"
    "  - More volume comments: suggest 'SurfPatch -vol'\n"
    "\n"
    "1.12 March 10, 2008  [rickr]\n"
    "  - Averages did _not_ include any -cmask option (so were too small)\n"
    "    (noticed by M Beauchamp)\n"
    "1.13 October 6, 2009 [ziad]\n"
    "  - Added volume estimation with approach based on Gauss' theorem\n"
    "\n"
    "----------------------------------------------------------------------\n";

/*----------------------------------------------------------------------
 * todo:
 *----------------------------------------------------------------------
*/

#include "mrilib.h"
#include "SUMA_suma.h"
#include "SUMA_SurfMeasures.h"

extern void machdep( void );


/* globals */
SUMA_SurfaceViewer * SUMAg_SVv = NULL;  /* array of Surf View structs   */
int                  SUMAg_N_SVv = 0;   /* length of SVv array          */
SUMA_DO            * SUMAg_DOv = NULL;  /* array of Displayable Objects */
int                  SUMAg_N_DOv = 0;   /* length of DOv array          */
SUMA_CommonFields  * SUMAg_CF = NULL;   /* info common to all viewers   */

/* these must match smeasure_codes_e enum */
char * g_sm_names[] = { "none", "ang_norms", "ang_ns_A", "ang_ns_B",
			"coord_A", "coord_B", "n_area_A", "n_area_B",
			"n_avearea_A", "n_avearea_B", "n_ntri",
			"node_vol", "node_volg", "nodes", "norm_A", "norm_B", "thick" };

char * g_sm_desc[] = { 
    "invalid function",
 	 "angular difference between normals",
	 "angular diff between segment and first norm",
	 "angular diff between segment and second norm",
	 "xyz coordinates of node on first surface",
	 "xyz coordinates of node on second surface",
	 "associated node area on first surface",
	 "associated node area on second surface",
	 "for each node, average area of triangles (surf A)",
	 "for each node, average area of triangles (surf B)",
	 "for each node, number of associated triangles",
	 "associated node volume between surfaces",
	 "associated node volume between surfaces via Gauss' theorem",
	 "node number",
	 "vector of normal at node on first surface",
	 "vector of normal at node on second surface",
	 "distance between surfaces along segment" };

/*----------------------------------------------------------------------*/

int main ( int argc, char * argv[] )
{
    param_t p;
    opts_t  opts;
    int     rv;

    /* note initial time */
    (void) COX_clock_time();

    mainENTRY("SurfMeasures main");
    machdep();
    AFNI_logger(PROG_NAME,argc,argv);

    if ( (rv = init_options(&opts, argc, argv)) != 0 ) {
	if( rv > 0 ) return 0;
        else         return 1;
    }

    if ( opts.debug > 1 )
	fprintf(stderr,"-- timing: init opts         : time = %.3f\n",
		COX_clock_time());

    if ( (rv = validate_options(&opts, &p)) != 0 )
	return rv;

    if ( opts.debug > 1 )
	fprintf(stderr,"-- timing: validate opts     : time = %.3f\n",
		COX_clock_time());

    if ( (rv = get_surf_data(&opts, &p)) != 0 )
	return rv;

    if ( opts.debug > 1 )
	fprintf(stderr,"-- timing: get surf data     : time = %.3f\n",
		COX_clock_time());

    if ( (rv = write_output(&opts, &p)) != 0 )
	return rv;

    if (p.out_dset) { /* add history and write out dset */
      char *oname = NULL;
      SUMA_AddNgrHist (p.out_dset->ngr, "SurfMeasures", argc, argv);
      if (!(oname = SUMA_WriteDset_s(opts.out_file, p.out_dset, 
                                     SUMA_NO_DSET_FORMAT, 
                                     THD_ok_overwrite(), 0))) {
         fprintf(stderr,"** failed to write %s\n", opts.out_file);
         return 1;                             
      }
      SUMA_free(oname);
    }
    
    if ( opts.debug > 1 )
	fprintf(stderr,"-- timing: write outfile     : time = %.3f\n",
		COX_clock_time());

    final_cleanup(&opts, &p);

    if ( opts.debug > 1 )
	fprintf(stderr,"-- timing: final clean up    : time = %.3f\n",
		COX_clock_time());

    return 0;
}

/*----------------------------------------------------------------------
 * prep output dset
 *
 *----------------------------------------------------------------------
*/
int prep_output_dset( opts_t * opts, param_t * p )
{
   static char FuncName[]={"prep_output_dset"};
   int fnum=0;
   
ENTRY("prep_output_dset");
   
   if (!opts->out_file) RETURN(0);
   
   if (!(p->out_dset = SUMA_CreateDsetPointer(opts->out_file, SUMA_NODE_BUCKET,
                                     NULL, NULL, p->nnodes))) {
      SUMA_S_Err("Failed to create new dset");
      RETURN(1);     
   }
   if (!SUMA_AddDsetNelCol ( p->out_dset, "Node Index", 
                             SUMA_NODE_INDEX, (void *)p->nodes, NULL, 1)) {
      SUMA_S_Err("Failed to add node index column");
      SUMA_FreeDset(p->out_dset); p->out_dset=NULL;
      RETURN(1);                                 
   }
   p->colmap = (int *)calloc(p->F->nused, sizeof(int));

         for (fnum = 0; fnum < p->F->nused; fnum++)
	{
       p->colmap[fnum] = SDSET_VECNUM(p->out_dset); 

       switch(p->F->codes[fnum])
	    {
		default:
		    fprintf(stderr,"** bad output Info code %d\n",p->F->codes[fnum]);
		    p->colmap[fnum]=-1; 
          break;

		case E_SM_ANG_NORMS:
		    if (!SUMA_AddDsetNelCol ( p->out_dset, "NormsAngle", 
                             SUMA_NODE_PHASE, (void *)p->nodes, NULL, 1)) {
             SUMA_S_Err("Failed to add column");
             SUMA_FreeDset(p->out_dset); p->out_dset=NULL;
             RETURN(1);                                 
          }
		    break;

		case E_SM_ANG_NS_A:
		    if (!SUMA_AddDsetNelCol ( p->out_dset, "Norm2ASegAngle", 
                             SUMA_NODE_PHASE, (void *)p->nodes, NULL, 1)) {
             SUMA_S_Err("Failed to add column");
             SUMA_FreeDset(p->out_dset); p->out_dset=NULL;
             RETURN(1);                                 
          }
		    break;

		case E_SM_ANG_NS_B:
		    if (!SUMA_AddDsetNelCol ( p->out_dset, "Norm2BSegAngle", 
                             SUMA_NODE_PHASE, (void *)p->nodes, NULL, 1)) {
             SUMA_S_Err("Failed to add column");
             SUMA_FreeDset(p->out_dset); p->out_dset=NULL;
             RETURN(1);                                 
          }
		    break;

		case E_SM_COORD_A:
          if (!SUMA_AddDsetNelCol ( p->out_dset, "XcoordA", 
                             SUMA_NODE_X, (void *)p->nodes, NULL, 1)) {
             SUMA_S_Err("Failed to add column");
             SUMA_FreeDset(p->out_dset); p->out_dset=NULL;
             RETURN(1);                                 
          }
          if (!SUMA_AddDsetNelCol ( p->out_dset, "YcoordA", 
                             SUMA_NODE_Y, (void *)p->nodes, NULL, 1)) {
             SUMA_S_Err("Failed to add column");
             SUMA_FreeDset(p->out_dset); p->out_dset=NULL;
             RETURN(1);                                 
          }
          if (!SUMA_AddDsetNelCol ( p->out_dset, "ZcoordA", 
                             SUMA_NODE_Z, (void *)p->nodes, NULL, 1)) {
             SUMA_S_Err("Failed to add column");
             SUMA_FreeDset(p->out_dset); p->out_dset=NULL;
             RETURN(1);                                 
          }
		    break;

		case E_SM_COORD_B:
		    if (!SUMA_AddDsetNelCol ( p->out_dset, "XcoordB", 
                             SUMA_NODE_X, (void *)p->nodes, NULL, 1)) {
             SUMA_S_Err("Failed to add column");
             SUMA_FreeDset(p->out_dset); p->out_dset=NULL;
             RETURN(1);                                 
          }
          if (!SUMA_AddDsetNelCol ( p->out_dset, "YcoordB", 
                             SUMA_NODE_Y, (void *)p->nodes, NULL, 1)) {
             SUMA_S_Err("Failed to add column");
             SUMA_FreeDset(p->out_dset); p->out_dset=NULL;
             RETURN(1);                                 
          }
          if (!SUMA_AddDsetNelCol ( p->out_dset, "ZcoordB", 
                             SUMA_NODE_Z, (void *)p->nodes, NULL, 1)) {
             SUMA_S_Err("Failed to add column");
             SUMA_FreeDset(p->out_dset); p->out_dset=NULL;
             RETURN(1);                                 
          }
		    break;

		case E_SM_N_AREA_A:
          if (!SUMA_AddDsetNelCol ( p->out_dset, "AreaA", 
                             SUMA_NODE_AREA, (void *)p->nodes, NULL, 1)) {
             SUMA_S_Err("Failed to add column");
             SUMA_FreeDset(p->out_dset); p->out_dset=NULL;
             RETURN(1);                                 
          }
          break;

		case E_SM_N_AREA_B:
		    if (!SUMA_AddDsetNelCol ( p->out_dset, "AreaB", 
                             SUMA_NODE_AREA, (void *)p->nodes, NULL, 1)) {
             SUMA_S_Err("Failed to add column");
             SUMA_FreeDset(p->out_dset); p->out_dset=NULL;
             RETURN(1);                                 
          }
          break;

      case E_SM_N_AVEAREA_A:
		    if (!SUMA_AddDsetNelCol ( p->out_dset, "AvgAreaA", 
                             SUMA_NODE_AREA, (void *)p->nodes, NULL, 1)) {
             SUMA_S_Err("Failed to add column");
             SUMA_FreeDset(p->out_dset); p->out_dset=NULL;
             RETURN(1);                                 
          }
          break;

      case E_SM_N_AVEAREA_B:
		    if (!SUMA_AddDsetNelCol ( p->out_dset, "AvgAreaB", 
                             SUMA_NODE_AREA, (void *)p->nodes, NULL, 1)) {
             SUMA_S_Err("Failed to add column");
             SUMA_FreeDset(p->out_dset); p->out_dset=NULL;
             RETURN(1);                                 
          }
          break;

      case E_SM_NTRI:
		    if (!SUMA_AddDsetNelCol ( p->out_dset, "NTri", 
                             SUMA_NODE_INT, (void *)p->nodes, NULL, 1)) {
             SUMA_S_Err("Failed to add column");
             SUMA_FreeDset(p->out_dset); p->out_dset=NULL;
             RETURN(1);                                 
          }
          break;

      case E_SM_NODES:
		    if (!SUMA_AddDsetNelCol ( p->out_dset, "Index", 
                             SUMA_NODE_INT, (void *)p->nodes, NULL, 1)) {
             SUMA_S_Err("Failed to add column");
             SUMA_FreeDset(p->out_dset); p->out_dset=NULL;
             RETURN(1);                                 
          }
		    break;

		case E_SM_NODE_VOL:
		    if (!SUMA_AddDsetNelCol ( p->out_dset, "Volume(Biased)", 
                             SUMA_NODE_VOLUME, (void *)p->nodes, NULL, 1)) {
             SUMA_S_Err("Failed to add column");
             SUMA_FreeDset(p->out_dset); p->out_dset=NULL;
             RETURN(1);                                 
          } 
          break;

		case E_SM_NODE_VOLG:
		    if (!SUMA_AddDsetNelCol ( p->out_dset, "Volume", 
                             SUMA_NODE_VOLUME, (void *)p->nodes, NULL, 1)) {
             SUMA_S_Err("Failed to add column");
             SUMA_FreeDset(p->out_dset); p->out_dset=NULL;
             RETURN(1);                                 
          } 
          break;

		case E_SM_NORM_A:
		    if (!SUMA_AddDsetNelCol ( p->out_dset, "XNormA", 
                             SUMA_NODE_X, (void *)p->nodes, NULL, 1)) {
             SUMA_S_Err("Failed to add column");
             SUMA_FreeDset(p->out_dset); p->out_dset=NULL;
             RETURN(1);                                 
          }
          if (!SUMA_AddDsetNelCol ( p->out_dset, "YNormA", 
                             SUMA_NODE_Y, (void *)p->nodes, NULL, 1)) {
             SUMA_S_Err("Failed to add column");
             SUMA_FreeDset(p->out_dset); p->out_dset=NULL;
             RETURN(1);                                 
          }
          if (!SUMA_AddDsetNelCol ( p->out_dset, "ZNormA", 
                             SUMA_NODE_Z, (void *)p->nodes, NULL, 1)) {
             SUMA_S_Err("Failed to add column");
             SUMA_FreeDset(p->out_dset); p->out_dset=NULL;
             RETURN(1);                                 
          } 
          break;

		case E_SM_NORM_B:
		    if (!SUMA_AddDsetNelCol ( p->out_dset, "XNormB", 
                             SUMA_NODE_X, (void *)p->nodes, NULL, 1)) {
             SUMA_S_Err("Failed to add column");
             SUMA_FreeDset(p->out_dset); p->out_dset=NULL;
             RETURN(1);                                 
          }
          if (!SUMA_AddDsetNelCol ( p->out_dset, "YNormB", 
                             SUMA_NODE_Y, (void *)p->nodes, NULL, 1)) {
             SUMA_S_Err("Failed to add column");
             SUMA_FreeDset(p->out_dset); p->out_dset=NULL;
             RETURN(1);                                 
          }
          if (!SUMA_AddDsetNelCol ( p->out_dset, "ZNormB", 
                             SUMA_NODE_Z, (void *)p->nodes, NULL, 1)) {
             SUMA_S_Err("Failed to add column");
             SUMA_FreeDset(p->out_dset); p->out_dset=NULL;
             RETURN(1);                                 
          } 
          break;

		case E_SM_THICK:
		    if (!SUMA_AddDsetNelCol ( p->out_dset, "Thickness", 
                            SUMA_NODE_THICKNESS, (void *)p->nodes, NULL, 1)) {
             SUMA_S_Err("Failed to add column");
             SUMA_FreeDset(p->out_dset); p->out_dset=NULL;
             RETURN(1);                                 
          }
          break;
	    }
	}

   RETURN(0);

}

/*----------------------------------------------------------------------
 * write_output
 *
 *----------------------------------------------------------------------
*/
int write_output( opts_t * opts, param_t * p )
{
    SUMA_SurfaceObject * so0, * so1;

    THD_fvec3   p0, p1;
    double      tarea0, tarea1, tvolume, tvolumeg;
    double      dist, min_dist, max_dist, tdist;
    double      atn = 0.0, atna = 0.0, atnb = 0.0;  /* angle totals */
    float       fvn, fva, fvb;
    float     * fp0, * fp1, *fv = NULL;
    float     * norms0, * norms1;
    float       ave_dist, fval;
    int         c, fnum, node, nindex;
    int       * fcodes, * iv=NULL;
    int         skipped = 0, tot_nodes;
    
ENTRY("write_output");

    so0 = p->S.slist[0];
    so1 = p->S.slist[1];

    /* just a simple case for now */

    print_column_headers(opts, p);

    /* initialize some pointers */
    fp0    = so0->NodeList;
    norms0 = so0->NodeNormList;

    /* if we have only one surface, just init fp1 to that */
    if ( p->S.nsurf > 1 )
    {
	fp1    = so1->NodeList;
	norms1 = so1->NodeNormList;
    }
    else
    {
	fp1    = fp0;
	norms1 = norms0;
    }

   
    if (prep_output_dset(opts, p)) { /* create output dset, if necessary */
       fprintf(stderr,"** Failed to initialize dset\n");
       RETURN(1);
    }

    fcodes = p->F->codes;	/* for convenience */

    tdist    = 0.0;              /* for total distance over nodes */
    tarea0   = 0.0;		/* for total area computation   */
    tarea1   = 0.0;		/* for total area computation  */
    tvolume  = 0.0;		/* for total volume           */
    tvolumeg = 0.0;		/* for total volume           */

    min_dist = 9999.0;
    max_dist = 0.0;
    for (nindex = 0; nindex < p->nnodes; nindex++)
    {
	if ( p->cmask && !p->cmask[nindex] )
	{
	    skipped++;
	    continue;
	}

	node = p->nodes[nindex];

	memcpy(p0.xyz, fp0+3*node, 3*sizeof(float));
	memcpy(p1.xyz, fp1+3*node, 3*sizeof(float));

	dist = dist_fn(3, p0.xyz, p1.xyz);
	if ( dist < min_dist ) min_dist = dist;
	if ( dist > max_dist ) max_dist = dist;
	tdist += dist;

	/* keep track of the total areas and volume */
	if ( p->S.narea[0] )
	    tarea0  += p->S.narea[0][node];

	if ( p->S.narea[1] )
	    tarea1  += p->S.narea[1][node];

	if ( p->S.nvol )
	    tvolume += p->S.nvol[node];
	
   if ( p->S.nvolg )
	    tvolumeg += p->S.nvolg[node];

	if (p->outfp) fputc(' ', p->outfp);


	for (fnum = 0; fnum < p->F->nused; fnum++)
	{
	    switch(fcodes[fnum])
	    {
		default:
		    fprintf(stderr,"** bad output Info code %d\n",fcodes[fnum]);
		    break;

		case E_SM_ANG_NORMS:
		    fvn = vector_angle(norms0 + 3*node, norms1 + 3*node);
		    if (p->outfp) fprintf(p->outfp,"  %10s", MV_format_fval(fvn));
		    atn += fvn;
          if (p->out_dset) { 
            fv = (float *)p->out_dset->dnel->vec[p->colmap[fnum]];  
		      fv[nindex]=fvn;
          }
          break;

		case E_SM_ANG_NS_A:
		    fva = norm2seg_angle(&p0, &p1, norms0 + 3*node);
		    if (p->outfp) fprintf(p->outfp,"  %10s", MV_format_fval(fva));
		    atna += fva;
          if (p->out_dset) { 
            fv = (float *)p->out_dset->dnel->vec[p->colmap[fnum]];  
		      fv[nindex]=fva;
          }
		    break;

		case E_SM_ANG_NS_B:
		    fvb = norm2seg_angle(&p0, &p1, norms1 + 3*node);
		    if (p->outfp) fprintf(p->outfp,"  %10s", MV_format_fval(fvb));
		    atnb += fvb;
		    if (p->out_dset) { 
            fv = (float *)p->out_dset->dnel->vec[p->colmap[fnum]];  
		      fv[nindex]=fvb;
          }
          break;

		case E_SM_COORD_A:
		    for (c = 0; c < 3; c++) 
			if (p->outfp) fprintf(p->outfp,"  %10s",
				MV_format_fval(fp0[3*node+c]));
		    if (p->out_dset) { 
            for (c = 0; c < 3; c++) {
               fv = (float *)p->out_dset->dnel->vec[p->colmap[fnum]+c];  
		         fv[nindex]=fp0[3*node+c];
            }
          }
          break;

		case E_SM_COORD_B:
		    for (c = 0; c < 3; c++)
			if (p->outfp) fprintf(p->outfp,"  %10s",
				MV_format_fval(fp1[3*node+c]));
		    if (p->out_dset) { 
            for (c = 0; c < 3; c++) {
               fv = (float *)p->out_dset->dnel->vec[p->colmap[fnum]+c];  
		         fv[nindex]=fp1[3*node+c];
            }
          }
          break;

		case E_SM_N_AREA_A:
		    if (p->outfp) fprintf(p->outfp,"  %10s",
			    MV_format_fval(p->S.narea[0][node]));
		    if (p->out_dset) { 
            fv = (float *)p->out_dset->dnel->vec[p->colmap[fnum]];  
		      fv[nindex]=p->S.narea[0][node];
          }
          break;

		case E_SM_N_AREA_B:
		    if (p->outfp) fprintf(p->outfp,"  %10s",
			    MV_format_fval(p->S.narea[1][node]));
		    if (p->out_dset) { 
            fv = (float *)p->out_dset->dnel->vec[p->colmap[fnum]];  
		      fv[nindex]=p->S.narea[1][node];
          }
          break;

		case E_SM_N_AVEAREA_A:
		    fval = 3 * p->S.narea[0][node] / so0->MF->N_Memb[node];
		    if (p->outfp) fprintf(p->outfp,"  %10s", MV_format_fval(fval));
		    if (p->out_dset) { 
            fv = (float *)p->out_dset->dnel->vec[p->colmap[fnum]];  
		      fv[nindex]=fval;
          }
          break;

		case E_SM_N_AVEAREA_B:
		    fval = 3 * p->S.narea[1][node] / so1->MF->N_Memb[node];
		    if (p->outfp) fprintf(p->outfp,"  %10s", MV_format_fval(fval));
		    if (p->out_dset) { 
            fv = (float *)p->out_dset->dnel->vec[p->colmap[fnum]];  
		      fv[nindex]=fval;
          }
          break;

		case E_SM_NTRI:
		    if (p->outfp) fprintf(p->outfp,"  %10d", so0->MF->N_Memb[node]);
		    if (p->out_dset) { 
            iv = (int *)p->out_dset->dnel->vec[p->colmap[fnum]];  
		      iv[nindex]=so0->MF->N_Memb[node];
          }
          break;

		case E_SM_NODES:
		    if (p->outfp) fprintf(p->outfp,"  %10d", node);
		    if (p->out_dset) { 
            iv = (int *)p->out_dset->dnel->vec[p->colmap[fnum]];  
		      iv[nindex]=node;
          }
          break;

		case E_SM_NODE_VOL:
		    if (p->outfp) 
            fprintf(p->outfp,"  %10s", MV_format_fval(p->S.nvol[node]));
		    if (p->out_dset) { 
            fv = (float *)p->out_dset->dnel->vec[p->colmap[fnum]];  
		      fv[nindex]=p->S.nvol[node];
          }
          break;

		case E_SM_NODE_VOLG:
		    if (p->outfp) 
            fprintf(p->outfp,"  %10s", MV_format_fval(p->S.nvolg[node]));
		    if (p->out_dset) { 
            fv = (float *)p->out_dset->dnel->vec[p->colmap[fnum]];  
		      fv[nindex]=p->S.nvolg[node];
          }
          break;

		case E_SM_NORM_A:
		    if ( norms0 ) {
			for (c = 0; c < 3 && p->outfp; c++)
			    fprintf(p->outfp,"  %10s",
				    MV_format_fval(norms0[3*node+c]));
		      if (p->out_dset) { 
               for (c = 0; c < 3; c++) {
                  fv = (float *)p->out_dset->dnel->vec[p->colmap[fnum]+c];  
		            fv[nindex]=norms0[3*node+c];
               }
            } 
          }
          break;

		case E_SM_NORM_B:
		    if ( norms1 ) {
			for (c = 0; c < 3 && p->outfp; c++)
			    fprintf(p->outfp,"  %10s",
				    MV_format_fval(norms1[3*node+c]));
		      if (p->out_dset) { 
               for (c = 0; c < 3; c++) {
                  fv = (float *)p->out_dset->dnel->vec[p->colmap[fnum]+c];  
		            fv[nindex]=norms1[3*node+c];
               }
            } 
          }
          break;

		case E_SM_THICK:
		    if (p->outfp) fprintf(p->outfp,"  %10s", MV_format_fval(dist));
		    if (p->out_dset) { 
            fv = (float *)p->out_dset->dnel->vec[p->colmap[fnum]];  
		      fv[nindex]=dist;
          }
          break;
	    }
	}

	if (p->outfp) fputc('\n', p->outfp);

	if ( node == opts->dnode && opts->debug > 0 )
	{
	    fprintf(stderr,"-- dnode %d:\n", node);
	    disp_f3_point("-- p0    = ", fp0+3*node);
	    disp_f3_point("-- p1    = ", fp1+3*node);

	    if ( atn > 0.0 || atna > 0.0 || atnb > 0.0 )
	    {
		disp_f3_point("-- normA = ", norms0 + 3*node);
		disp_f3_point("-- normB = ", norms1 + 3*node);
	    }
	}
    }

    /* update dset column range */
    if (p->out_dset) SUMA_UpdateDsetColRange(p->out_dset, -1);
    
    /* Ick, averages should not include skipped nodes!  10 Mar 2008 [rickr] */
    tot_nodes = p->nnodes - skipped;
    if( tot_nodes <= 0 ) {
        printf("** no nodes remaining for averages (skipped = %d)!\n",skipped);
        RETURN(0);
    }

    ave_dist = tdist/tot_nodes;

    if ( opts->info )
	printf("----------------------------------------------------------\n");

    if ( opts->info & ST_INFO_AREA )
    {
	if ( p->S.narea[0] )
	    printf("-- total area 0 = %.1f\n", tarea0);

    	if ( p->S.nsurf > 1 )
	    printf("-- total area 1 = %.1f\n", tarea1);
    }

    if ( (opts->info & ST_INFO_NORMS) &&
	 (atn > 0.0 || atna > 0.0 || atnb > 0.0) )
    {
	printf( "-- ave. angles to normals: (nA_nB, sA, sB) = "
		"(%.4f, %.4f, %.4f)\n",
		atn/tot_nodes, atna/tot_nodes, atnb/tot_nodes);
    }

    if ( (opts->info & ST_INFO_THICK) && (p->S.nsurf > 1) )
	printf( "-- thickness: (min, ave, max) = (%.5f, %.5f, %.5f)\n",
		min_dist, ave_dist, max_dist);

    if ( (opts->info & ST_INFO_VOL) && p->S.nvol )
    {
	printf("-- total volume = %.1f\n", tvolume);
	if ( opts->debug > 1 )
	    printf("-- ave dist * (area0, ave, area1) = (%.1f, %.1f, %.1f)\n",
		ave_dist*tarea0, ave_dist*(tarea0+tarea1)/2, ave_dist*tarea1);
    }

    if ( (opts->info & ST_INFO_VOLG) && p->S.nvolg )
    {
	printf("-- total volume with Gauss Theorem = %.1f\n", tvolumeg);
	if ( opts->debug > 1 )
	    printf("-- ave dist * (area0, ave, area1) = (%.1f, %.1f, %.1f)\n",
		ave_dist*tarea0, ave_dist*(tarea0+tarea1)/2, ave_dist*tarea1);
    }

    if ( p->cmask )
	printf("-- from cmask, nodes skipped = %d\n", skipped);

    RETURN(0);
}


/*----------------------------------------------------------------------
 * norm2seg_angle		- return the angle between segment and norm
 *----------------------------------------------------------------------
*/
float norm2seg_angle( THD_fvec3 * p0, THD_fvec3 * p1, float * norm )
{
    float seg[3];
    int   c;

ENTRY("norm2seg_angle");

    for ( c = 0; c < 3; c++ )
	seg[c] = p1->xyz[c] - p0->xyz[c];

    RETURN(vector_angle(seg, norm));
}


/*----------------------------------------------------------------------
 * fvec_magnitude			- compute magnitude of float vector
 *----------------------------------------------------------------------
*/
float fvec_magnitude( float * v, int length )
{
    double sums;
    int    c;

ENTRY("fvec_magnitude");

    sums = 0.0;
    for ( c = 0; c < length; c++ )
	sums += v[c] * v[c];

    RETURN(sqrt(sums));
}


/*----------------------------------------------------------------------
 * vector_angle		- return the angle between the vectors
 *
 * if either magnitude is 0, return 0
 * else, use dot product definition
 *----------------------------------------------------------------------
*/
float vector_angle( float * v0, float * v1 )
{
    double mag0, mag1, dot, ratio, angle;

ENTRY("vector_angle");

    mag0 = fvec_magnitude(v0, 3);
    mag1 = fvec_magnitude(v1, 3);
    dot  = v0[0]*v1[0] + v0[1]*v1[1] + v0[2]*v1[2];

    if ( mag0 == 0.0 || mag1 == 0.0 )
	RETURN(0.0);

    ratio = dot / (mag0 * mag1);
    RANGE(-1.0, ratio, 1.0);

    angle = acos(ratio);

    /* keep angle in [0,PI/2] */
    if ( 2 * angle > ST_PI )
	angle = PI - angle;

    RETURN(angle);
}


/*----------------------------------------------------------------------
 * print_column_headers
 *----------------------------------------------------------------------
*/
int print_column_headers( opts_t * opts, param_t * p )
{
    int c, c2, num2print;

ENTRY("print_column_headers");
    
    if (!p->outfp) RETURN(0);
    
    fputc('#', p->outfp);
    for (c = 0; c < p->F->nused; c++ )
    {
	if ( ( p->F->codes[c] == E_SM_COORD_A ) ||
	     ( p->F->codes[c] == E_SM_COORD_B ) ||
	     ( p->F->codes[c] == E_SM_NORM_A  ) ||
	     ( p->F->codes[c] == E_SM_NORM_B  ) )
	    num2print = 3;
	else
	    num2print = 1;

	for (c2 = 0; c2 < num2print; c2++)
	    if ( num2print > 1 )
		fprintf(p->outfp, " %8s[%d]", p->F->names[c],c2);
	    else
		fprintf(p->outfp, "  %10s", p->F->names[c]);
    }
    fputc('\n', p->outfp);

    fputc('#', p->outfp);
    for (c = 0; c < p->F->nused; c++ )
    {
	if ( ( p->F->codes[c] == E_SM_COORD_A ) ||
	     ( p->F->codes[c] == E_SM_COORD_B ) ||
	     ( p->F->codes[c] == E_SM_NORM_A  ) ||
	     ( p->F->codes[c] == E_SM_NORM_B  ) )
	    num2print = 3;
	else
	    num2print = 1;

	for (c2 = 0; c2 < num2print; c2++)
	    fprintf(p->outfp, "  ----------");
    }
    fputc('\n', p->outfp);

    RETURN(0);
}


/*----------------------------------------------------------------------
 * verify_surf_t
 *
 *----------------------------------------------------------------------
*/
int verify_surf_t( opts_t * opts, param_t * p )
{
    int c;

ENTRY("verify_surf_t");

    if ( p->S.nsurf < 1 )
    {
	fprintf(stderr,"** no surfaces found\n");
	if ( opts->debug > 0 )
	    disp_surf_t("-- empty? ", &p->S);
	RETURN(-1);
    }

    if ( p->S.nsurf > 1 )
    {
	for ( c = 1; c < p->S.nsurf; c++ )
	    if ( p->S.slist[c]->N_Node != p->S.nnodes )
	    {
		fprintf(stderr,"** surface %d has %d nodes (should be %d)\n",
			c, p->S.slist[c]->N_Node, p->S.nnodes);
		RETURN(-1);
	    }
    }

    /* verify nodes and cmask */
    if ( ! p->nodes )			/* if empty, create the trivial list */
    {
	p->nnodes = p->S.nnodes;
	p->nodes  = (int *)malloc(p->nnodes * sizeof(int));
	ALLOC_CHECK(p->nodes, "int", p->S.nnodes);

	/* now fill the list with trivial indices */
	for ( c = 0; c < p->nnodes; c++ )
	    p->nodes[c] = c;
    }
    else				/* verify that the indices are valid */
    {
	for ( c = 0; c < p->nnodes; c++ )
	    if ( (p->nodes[c] < 0) || (p->nodes[c] >= p->S.nnodes) )
	    {
		fprintf(stderr,"** error: node list index %d (value = %d):"
		               "          outside valid range [0,%d]\n",
			c, p->nodes[c], p->S.nnodes-1);
		RETURN(-1);
	    }
    }

    /* in any case, if there is a mask, the length should match nnodes */
    if ( p->cmask && (p->ncmask != p->nnodes) )
    {
	fprintf(stderr,"** cmask and node list lengths differ (%d, %d)\n",
		p->ncmask, p->nnodes);
	RETURN(-1);
    }

    if ( opts->debug > 1 )
    {
	disp_param_t("-- surf params verified: ", p);
	disp_surf_t ("-- surf params verified: ", &p->S);
    }

    if ( validate_option_lists(opts, p) != 0 )
	RETURN(-1);

    RETURN(0);
}


/*----------------------------------------------------------------------
 * get_surf_data
 *
 *----------------------------------------------------------------------
*/
int get_surf_data( opts_t * opts, param_t * p )
{
    int rv;

ENTRY("get_surf_data");

    rv = spec2SUMA(&p->S.spec, opts);
    if ( rv != 0 )
	RETURN(rv);

    if ( opts->debug > 1 )
	fprintf(stderr,"-- timing: Surf (spec2SUMA)  : time = %.3f\n",
		COX_clock_time());

    if ( (rv = all_mappable_surfs(opts, p)) != 0 )
	RETURN(rv);

    if ( opts->debug > 1 )
	fprintf(stderr,"-- timing: Surf (all_map)    : time = %.3f\n",
		COX_clock_time());

    if ( (rv = verify_surf_t(opts, p)) != 0)
	RETURN(rv);

    if ( opts->debug > 1 )
	fprintf(stderr,"-- timing: Surf (verify surf): time = %.3f\n",
		COX_clock_time());

    if ( (rv = get_surf_measures(opts, p)) != 0)
	RETURN(rv);

    if ( opts->debug > 1 )
	fprintf(stderr,"-- timing: Surf (get measure): time = %.3f\n",
		COX_clock_time());

    RETURN(0);
}


/*----------------------------------------------------------------------
 * get_surf_measures
 *----------------------------------------------------------------------
*/
int get_surf_measures( opts_t * opts, param_t * p )
{
    int * fcodes;
    int   c, debug;
    int   geta, getb;

ENTRY("get_surf_measures");

    geta = getb = 0;
    debug = opts->debug > 2;

    if ( (opts->info & ST_INFO_VOL) || 
         (opts->info & ST_INFO_VOLG) )
    {
	geta = getb = 1;
    }
    else		/* maybe there are functions that need them */
    {
	fcodes = p->F->codes;
	for ( c = 0; c < p->F->nused; c++ )
	{
	    if ( fcodes[c] == E_SM_N_AREA_A )
		geta = 1;
	    else if ( fcodes[c] == E_SM_N_AREA_B )
		getb = 1;
	    else if ( fcodes[c] == E_SM_N_AVEAREA_A )
		geta = 1;
	    else if ( fcodes[c] == E_SM_N_AVEAREA_B )
		getb = 1;
	    else if ( fcodes[c] == E_SM_NODE_VOL )
	    {
		geta = getb = 1;
	    }
       else if ( fcodes[c] == E_SM_NODE_VOLG )
	    {
		geta = getb = 1;
	    }
	}
    }

    if ( geta ) 
    {
	if ( !SUMA_SurfaceMetrics_eng(p->S.slist[0], "PolyArea", NULL, debug,
		                      SUMAg_CF->DsetList) )
	{
	    fprintf(stderr,"** gsf: surface metrics A failure\n");
	    RETURN(-1);
	}

	if ( compute_node_areas(opts, p, 0) != 0 )	/* index 0 */
	    RETURN(-1);
    }

    if ( getb )
    {
	if ( p->S.nsurf < 2 )
	{
	    fprintf(stderr,"** gsf: functions requre 2 surfaces, failing...\n");
	    RETURN(-1);
	}

	if ( !SUMA_SurfaceMetrics_eng(p->S.slist[1], "PolyArea", NULL, debug,
		                      SUMAg_CF->DsetList) )
	{
	    fprintf(stderr,"** gsf: surface metrics B failure\n");
	    RETURN(-1);
	}

	if ( compute_node_areas(opts, p, 1) != 0 )  /* index 1 */
	    RETURN(-1);
    }

    if( geta && getb )
    {
	if ( surf_triangle_match(opts, p) != 0 )
	    RETURN(-1);
	if ( compute_face_vols(opts, p) != 0 )
	    RETURN(-1);
	if ( compute_node_vols(opts, p) != 0 )
	    RETURN(-1);
   if ( compute_node_vols_G(opts, p) != 0 )
	    RETURN(-1);
    }

    RETURN(0);
}


/*----------------------------------------------------------------------
 * surf_triangles_match	- check that the trangle lists match 
 *----------------------------------------------------------------------
*/
int surf_triangle_match( opts_t * opts, param_t * p )
{
    int   face, maxf;
    int * f0, * f1;

ENTRY("surf_triangle_match");

    f0 = p->S.slist[0]->FaceSetList;
    f1 = p->S.slist[1]->FaceSetList;

    maxf = p->S.slist[0]->N_FaceSet * 3;

    if ( !f0 || !f1 || maxf <= 0 )
    {
	fprintf(stderr,"** stm: bad face data (%p, %p, %d)\n", f0, f1, maxf);
	RETURN(-1);
    }

    for ( face = 0; face < maxf; face++ )
    {
	if (*f0 != *f1 )
	{
	    fprintf(stderr,"** face diff @ f3 = %d, *f0,*f1 = (%d,%d)\n",
		    face, *f0, *f1);
	    RETURN(-1);
	}
	f0++;  f1++;
    }

    if ( opts->debug > 1 )
	fprintf(stderr,"-- faces okay, woohoo!\n");

    RETURN(0);
}


#if 0		/* test_planar_sides */

/*----------------------------------------------------------------------
 * test_planar_sides	- check that all pentahedra have planar sides
 *----------------------------------------------------------------------
*/
int test_planar_sides( opts_t * opts, param_t * p )
{
    SUMA_SurfaceObject    * so;
    float                 * nodes0, * nodes1;
    float                   p1, p2, p3, max, mold;
    int                     a, b, c, mcount;
    int                     face, nfaces;
    int                   * flist;

ENTRY("test_planar_sides");

    flist  = p->S.slist[0]->FaceSetList;
    nfaces = p->S.slist[1]->N_FaceSet;

    nodes0 = p->S.slist[0]->NodeList;
    nodes1 = p->S.slist[1]->NodeList;

    if ( !flist || !nodes0 || !nodes1 || nfaces <= 0 )
    {
	fprintf(stderr,"** tps: bad face or node data (%p, %p, %p, %d)\n",
		flist, nodes0, nodes1, nfaces);
	RETURN(-1);
    }

    max    = 0.0;
    mold   = 1.0;		/* basic cutoff  */
    mcount = 0;			/* count updates */
    for ( face = 0; face < nfaces; face++ )
    {
	a = flist[0] * 3;		/* get the indices into NodeList */
	b = flist[1] * 3;
	c = flist[2] * 3;

	p1 = eval_planar_points(nodes0+a, nodes0+b, nodes1+a, nodes1+b);
	p2 = eval_planar_points(nodes0+b, nodes0+c, nodes1+b, nodes1+c);
	p3 = eval_planar_points(nodes0+c, nodes0+a, nodes1+c, nodes1+a);
	if (p1 > max) max = p1;
	if (p2 > max) max = p2;
	if (p3 > max) max = p3;

	if ( max > mold )
	{
	    mcount++;
	    if ( opts->debug > 2 )
	    {
		fprintf(stderr,"** new max (%d) for face %d, m = %f, m0 = %f\n",
			mcount, face, max, mold );
		disp_f3_point("  a0 = ", nodes0+a);
		disp_f3_point("  b0 = ", nodes0+b);
		disp_f3_point("  c0 = ", nodes0+c);
		disp_f3_point("  a1 = ", nodes1+a);
		disp_f3_point("  b1 = ", nodes1+b);
		disp_f3_point("  c1 = ", nodes1+c);
	    }
	    mold = max*1.5;
	}

	flist += 3;
    }

    if ( max > 1.0 )	/* rcr - what test to use? */
	RETURN(1);

    RETURN(0);
}


/*----------------------------------------------------------------------
 * planar_points		- check whether points are co-planer
 *
 * return 0 if false
 *----------------------------------------------------------------------
*/
float eval_planar_points( float * a, float * b, float * c, float * d )
{
ENTRY("planar_points");

    double d0[3], d1[3], d2[3];
    double xp[3];
    double result;

    d0[0] = c[0] - a[0];
    d0[1] = c[1] - a[1];
    d0[2] = c[2] - a[2];

    d1[0] = b[0] - a[0];
    d1[1] = b[1] - a[1];
    d1[2] = b[2] - a[2];

    d2[0] = d[0] - c[0];
    d2[1] = d[1] - c[1];
    d2[2] = d[2] - c[2];

    cross_product(xp, d1, d2);

    result = d0[0]*xp[0] + d0[1]*xp[1] + d0[2]*xp[2];

    RETURN(fabs(result));
}

#endif		/* test_planar_sides */


/*----------------------------------------------------------------------
 * cross_product		- return the cross product of u and v
 *----------------------------------------------------------------------
*/
int cross_product( double * res, double * u, double * v )
{
    res[0] = u[1]*v[2] - u[2]*v[1];
    res[1] = u[2]*v[0] - u[0]*v[2];
    res[2] = u[0]*v[1] - u[1]*v[0];

    return 0;
}


/*----------------------------------------------------------------------
 * dot_product		- return the dot product of u and v
 *----------------------------------------------------------------------
*/
double dot_product( double * u, double * v )
{
    double res;

    res  = u[0]*v[0];
    res += u[1]*v[1];
    res += u[2]*v[2];

    return res;
}


/*----------------------------------------------------------------------
 * compute_node_areas			- surface area for each node
 *
 * The area of a node is defined as one third of the area of the
 * associated triangles.
 *----------------------------------------------------------------------
*/
int compute_node_areas( opts_t * opts, param_t * p, int sindex )
{
    SUMA_SurfaceObject    * so;
    double                  sum;
    float                 * alist;
    int                   * flist;
    int                     node, c;

ENTRY("compute_node_areas");

    if ( sindex < 0 || sindex >= p->S.nsurf || sindex >= 2 )
    {
	fprintf(stderr,"** cna: surf index <%d> is out of range\n",sindex);
	RETURN(-1);
    }

    so = p->S.slist[sindex];		/* just for ease of typing */

    if ( ! so->PolyArea )
    {
	fprintf(stderr,"** cna: no PolyArea to compute from\n");
	RETURN(-1);
    }

    alist = (float *)malloc(p->S.nnodes*sizeof(float));
    ALLOC_CHECK(alist, "float", p->S.nnodes);
    p->S.narea[sindex] = alist;

    for ( node = 0; node < p->S.nnodes; node++ )
    {
	flist = so->MF->NodeMemberOfFaceSet[node];
	sum = 0.0;
	for (c = 0; c < so->MF->N_Memb[node]; c++)
	{
	    if ( flist[c] < 0 || flist[c] >= so->N_FaceSet )
	    {
		fprintf(stderr,"** cna: FaceSet mis-match flist,max = %d,%d\n",
			flist[c], so->N_FaceSet);
		free(p->S.narea[sindex]);
		RETURN(-1);
	    }

	    sum += so->PolyArea[flist[c]];
	}

	alist[node] = sum/3.0;

	if ( node == opts->dnode )
	    fprintf(stderr, "-- dnode %d: area = %s (%d faces, surf %s)\n",
		    node, MV_format_fval(alist[node]),
		    so->MF->N_Memb[node], CHECK_NULL_STR(so->Label));
    }

    RETURN(0);
}


/*----------------------------------------------------------------------
 * compute_node_vols			- get volume at each node
 *
 * Set each node's volume to one third the sum of the included
 * triangle face volumes.
 *----------------------------------------------------------------------
*/
int compute_node_vols( opts_t * opts, param_t * p )
{
    SUMA_SurfaceObject    * so;
    double		    sum;
    float                 * nvols;
    int                   * flist;
    int                     node, c;

ENTRY("compute_node_vols");

    so = p->S.slist[0];			/* just for ease of typing */

    nvols = (float *)malloc(p->S.nnodes*sizeof(float));
    ALLOC_CHECK(nvols, "float", p->S.nnodes);
    p->S.nvol = nvols;

    for ( node = 0; node < p->S.nnodes; node++ )
    {
	if ( node == opts->dnode && opts->debug > 1 )
	    fprintf(stderr,"-- dnode %d, facelist (%d) :\n        ",
		    node, so->MF->N_Memb[node]);
	flist = so->MF->NodeMemberOfFaceSet[node];
	sum = 0.0;
	for (c = 0; c < so->MF->N_Memb[node]; c++)
	{
	    if ( flist[c] < 0 || flist[c] >= so->N_FaceSet )
	    {
		fprintf(stderr,"** cnv: FaceSet mis-match flist,max = %d,%d\n",
			flist[c], so->N_FaceSet);
		free(p->S.nvol);
		RETURN(-1);
	    }
	    if ( node == opts->dnode && opts->debug > 1 )
		fprintf(stderr,"  %d", flist[c]);

	    sum += p->S.fvol[flist[c]];
	}

	nvols[node] = sum/3.0;

	if ( node == opts->dnode && opts->debug > 0 )
	    fprintf(stderr,"\n-- volume = %f\n", sum);
    }

    RETURN(0);
}

/*---------------------------------------------------------------------------
 * compute_node_vols_G			- get volume at each node via Gauss theorem. 
 *
 * Set each node's volume to one third of the chunk formed by the node and 
 * its immediate neighbors
 *---------------------------------------------------------------------------
*/
int compute_node_vols_G( opts_t * opts, param_t * p )
{
    SUMA_SurfaceObject     * so1, *so2, *sop;
    double		            sum, xform[4][4];
    float                  * nvols;
    int                    prob, node, i, i3, t3, D, D3, N, on3;
    int                    fn[1024];
    char                   sdbg[1024];
ENTRY("compute_node_vols_G");

    so1 = p->S.slist[0];			/* just for ease of typing */
    so2 = p->S.slist[1];			/* just for ease of typing */
    if (so1->FN->N_Neighb_max >= 1024) {
       fprintf(stderr,"** Lousy surface.\n");
       RETURN(-1);
    }
    
    nvols = (float *)malloc(p->S.nnodes*sizeof(float));
    ALLOC_CHECK(nvols, "float", p->S.nnodes);
    p->S.nvolg = nvols;

    for ( node = 0; node < p->S.nnodes; node++ )
    {
   /* inefficient to allocate every time, 
      but it is safe and plenty fast */
   sop = SUMA_Alloc_SurfObject_Struct(1);
   
	if ( node == opts->dnode && opts->debug > 1 )
	    fprintf(stderr,"-- dnode %d, facelist (%d) :\n        ",
		    node, so1->MF->N_Memb[node]);
   N = so1->FN->N_Neighb[node];/* number of node neighbors */
   D = N + 1; /* number of nodes on either side of patch */
   sop->N_Node = (N+1)*2;     /* number of nodes in patch */
   sop->N_FaceSet = 4*N;      /* number of facesets in patch */
   sop->NodeDim = 3;
   sop->FaceSetDim = 3;
   sop->NodeList = (float *)SUMA_calloc(sop->NodeDim*sop->N_Node, sizeof(float));
   sop->FaceSetList = (int *)SUMA_calloc(sop->FaceSetDim*sop->N_FaceSet, 
                                                                    sizeof(int));
   /* fill node coords See ZSS' Labbook NIH-5, pp63 */
   on3 = 3*node; D3 = 3*D;
   sop->NodeList[0] = so1->NodeList[on3  ]; /* Node O (oh)*/
   sop->NodeList[1] = so1->NodeList[on3+1]; 
   sop->NodeList[2] = so1->NodeList[on3+2];
   sop->NodeList[D3  ] = so2->NodeList[on3  ]; /* Node D */
   sop->NodeList[D3+1] = so2->NodeList[on3+1]; 
   sop->NodeList[D3+2] = so2->NodeList[on3+2];
   for (i=0; i<N; ++i) {
      i3 = 3*(i+1); D3=3*(D+i+1);
      on3 = 3*so1->FN->FirstNeighb[node][i];
      sop->NodeList[i3  ] = so1->NodeList[on3  ]; /* Node 1 */
      sop->NodeList[i3+1] = so1->NodeList[on3+1]; 
      sop->NodeList[i3+2] = so1->NodeList[on3+2];         
      sop->NodeList[D3  ] = so2->NodeList[on3  ]; /* Node D+1 */
      sop->NodeList[D3+1] = so2->NodeList[on3+1]; 
      sop->NodeList[D3+2] = so2->NodeList[on3+2];            
   }

   /* fill triangles */
   for (i=0; i<N; ++i) fn[i] = i+1;
   fn[N] = 1;
   t3 = 0;
   for (i=0; i<N; ++i) {   /* for each neighbor of node */
      sop->FaceSetList[t3] = 0      ;   ++t3;   /* top triangle */
      sop->FaceSetList[t3] = fn[i  ];   ++t3;
      sop->FaceSetList[t3] = fn[i+1];   ++t3;
      
      sop->FaceSetList[t3] =   fn[i  ]; ++t3;   /* first lateral */
      sop->FaceSetList[t3] = D+fn[i  ]; ++t3;
      sop->FaceSetList[t3] =   fn[i+1]; ++t3;
      
      sop->FaceSetList[t3] =   fn[i+1]; ++t3;   /* 2nd   lateral */
      sop->FaceSetList[t3] = D+fn[i  ]; ++t3;
      sop->FaceSetList[t3] = D+fn[i+1]; ++t3;   
      
      sop->FaceSetList[t3] = D        ; ++t3;   /* bottom triangle */
      sop->FaceSetList[t3] = D+fn[i+1]; ++t3;
      sop->FaceSetList[t3] = D+fn[i  ]; ++t3;
   }
   
   SUMA_RECOMPUTE_NORMALS(sop);  /* For speed,
      one could calculate the normals without, this macro.
      Some come straight out of so1 and so2, but speed is not
      a problem at all it seems. */
   if ((sum = SUMA_Mesh_Volume(sop, NULL, -1, 0, &prob)) < 0) sum = -sum;
   if (prob) { /* precision problem, wiggle surface and get back in there */
      SUMA_FillRandXform(xform, 12345, 2);
      SUMA_Apply_Coord_xform(sop->NodeList, sop->N_Node, sop->NodeDim,
                             xform, 0, NULL);
      SUMA_RECOMPUTE_NORMALS(sop);
      if ((sum = SUMA_Mesh_Volume(sop, NULL, -1, 0, &prob)) < 0) sum = -sum;
   }
   nvols[node] = sum/3.0;

	if ( node == opts->dnode && opts->debug > 0 ) {
       sprintf(sdbg, "volpatch_%d", node);
	    fprintf(stderr,"\n-- volumeG[%d] = %f, patch in %s\n", 
                        node, nvols[node], sdbg);
       SUMA_Save_Surface_Object_Wrap(sdbg, NULL, sop, 
                                     SUMA_PLY, SUMA_ASCII, NULL);
   }
   
   /* free surface to start anew */
   SUMA_Free_Surface_Object(sop);
   sop=NULL;
  
    }
   

    RETURN(0);
}

/*----------------------------------------------------------------------
 * compute_face_vols			- volume for each triangle face
 *
 * The volume corresponding to a triange face is the approximate volume
 * of the pentahedron formed by the pair of corresponding triangular faces.
 * It is computed (approximated) as the sum of three quadrahedrons, which
 * will be close to correct, and will give a very accurate total volume
 * (since the interior face volumes will perfectly fill the space).
 *----------------------------------------------------------------------
*/
int compute_face_vols( opts_t * opts, param_t * p )
{
    SUMA_SurfaceObject    * so0, *so1;
    double                  totalv;
    double                  v0, v1, v2;
    float                 * fvlist, * xyzn0, * xyzn1;
    float                   vmin, vmax;
    int                   * fp;
    int                     i0, i1, i2;
    int                     face, nnodes, test;

ENTRY("compute_face_vols");

    so0 = p->S.slist[0];
    so1 = p->S.slist[1];

    if ( !so0 || !so1 )
    {
	fprintf(stderr,"** cfv: missing SurfaceObject (%p, %p)\n", so0, so1 );
	RETURN(-1);
    }

    fp    = so0->FaceSetList;  /* we need only 1, as they are the same */
    xyzn0 = so0->NodeList;
    xyzn1 = so1->NodeList;
    
    if ( !fp || !xyzn0 || !xyzn1 )
    {
	fprintf(stderr,"** cfv: missing face set list data (%p,%p,%p)\n",
		fp, xyzn0, xyzn1);
	RETURN(-1);
    }

    fvlist = (float *)malloc(p->S.nfaces*sizeof(float));
    ALLOC_CHECK(fvlist, "float", p->S.nfaces);
    p->S.fvol = fvlist;
    
    vmin = 10000.0;  vmax = -1.0;
    nnodes = p->S.nnodes;
    totalv = 0.0;
    for ( face = 0; face < p->S.nfaces; face++ )
    {
	i0 = fp[0];  i1 = fp[1];  i2 = fp[2];

	if ( i0 < 0 || i1 < 0 || i2 < 0 ||
	     i0 >= nnodes || i1 >= nnodes || i2 >= nnodes )
	{
	    fprintf(stderr,"** cfv: face %d, index out of range [0,%d]\n"
		           "        indices are (%d,%d,%d)\n",
			   face, nnodes-1, i0, i1, i2);
	    RETURN(-1);
	}

	test = (i0 == opts->dnode || i1 == opts->dnode || i2 == opts->dnode);

	i0 *= 3;  i1 *= 3;  i2 *= 3;	/* node index -> (float*) index */

	v0 = tetra_volume( xyzn0+i0, xyzn0+i1, xyzn0+i2, xyzn1+i0 );
	v1 = tetra_volume( xyzn0+i1, xyzn0+i2, xyzn1+i0, xyzn1+i1 );
	v2 = tetra_volume( xyzn0+i2, xyzn1+i0, xyzn1+i1, xyzn1+i2 );

	if ( v0 < 0.0 || v1 < 0.0 || v2 < 0.0 ||
	     (opts->debug > 2 && test) )
	{
	    fprintf(stderr,"** cfv: check volume: %f = %f + %f + %f, face %d\n",
			   v0+v1+v2, v0, v1, v2, face );
	    fprintf(stderr,"        nodes %d, %d, %d\n", i0/3, i1/3, i2/3);

	    if ( v0 < 0.0 || v1 < 0.0 || v2 < 0.0 )
		RETURN(-1);
	}

	fvlist[face] = v0 + v1 + v2;
	totalv += fvlist[face];
	if ( fvlist[face] < vmin )  vmin = fvlist[face];
	if ( fvlist[face] > vmax )  vmax = fvlist[face];

	fp += 3;
    }

    if ( opts->debug > 0 )
    {
	fprintf(stderr,"++ total face volume = %f\n", totalv);
	if ( opts->debug > 1 )
	{
	    fprintf(stderr,"++ volumes: faces 0, 1 (of %d), vols = %f, %f\n",
		    p->S.nfaces, fvlist[0], fvlist[1]);
	    fprintf(stderr,"-- faces: vmin, vmax = %f, %f\n", vmin, vmax);
	}
    }

    RETURN(0);
}


/*----------------------------------------------------------------------
 * all_mappable_surfs		- fill surf_t struct, S
 *
 *----------------------------------------------------------------------
*/
int all_mappable_surfs( opts_t * opts, param_t * p )
{
    SUMA_SurfaceObject * so;
    int                  count;

ENTRY("all_mappable_surfs");

    p->S.slist = (SUMA_SurfaceObject **)malloc(SUMAg_N_DOv *
	                                       sizeof(SUMA_SurfaceObject *));
    ALLOC_CHECK(p->S.slist,"SUMA_SurfaceObject *",SUMAg_N_DOv);

    p->S.salloc = SUMAg_N_DOv;		/* number allocated for              */
    p->S.nsurf  = 0;			/* number of mappable surfaces found */

    for ( count = 0; count < SUMAg_N_DOv; count++ )
    {
	if ( ! SUMA_isSO(SUMAg_DOv[count]) )
	    continue;

	so = (SUMA_SurfaceObject *)SUMAg_DOv[count].OP;

/*	if ( ! SUMA_isINHmappable(so) )       -  deprecated  [v1.5] */

#if 0   /* do not require this [v1.9] */

	if ( ! so->AnatCorrect )
	{
	    if ( opts->debug )
		fprintf(stderr,"** warning: surface '%s' is not mappable, "
		        "skipping...\n", so->Label ? so->Label : "<unnamed>");
	    if ( opts->debug > 1 )
		fprintf(stderr,"** consider adding the following to the "
			       "surface definition in the spec file:\n"
			       "       Anatomical = Y\n");
	    continue;
	}
#endif

	if ( opts->debug > 1 )
	{
	    fprintf(stderr,"-------- surface #%d (%s) --------\n",
		    p->S.nsurf, so->Label ? so->Label : "<unnamed>");
	    if ( opts->debug > 3 )
		SUMA_Print_Surface_Object(so, stderr);
	}

	p->S.slist[p->S.nsurf] = so;
	p->S.nsurf++;
    }

    if ( p->S.nsurf > 0 )
    {
	p->S.nnodes = p->S.slist[0]->N_Node;
	p->S.nfaces = p->S.slist[0]->N_FaceSet;
    }

    if ( opts->debug )
	fprintf(stderr, "++ found %d mappable surfaces\n", p->S.nsurf);

    RETURN(0);
}


/*----------------------------------------------------------------------
 * spec2SUMA		- call the SUMA functions for reading surfaces
 *
 *----------------------------------------------------------------------
*/
int spec2SUMA( SUMA_SurfSpecFile * spec, opts_t * opts )
{
    int rv;

ENTRY("spec2SUMA");

    /* initialize common fields struct */
    SUMAg_CF = SUMA_Create_CommonFields();

    if ( SUMAg_CF == NULL )
    {
        fprintf( stderr, "** failed SUMA_Create_CommonFields(), exiting...\n" );
	RETURN(-1);
    }

    /* for SUMA type notifications */
    if ( opts->debug > 3 )
    {
	SUMAg_CF->MemTrace = 1;

	if ( opts->debug > 4 )
	    SUMAg_CF->InOut_Notify = 1;
    }

    SUMAg_DOv = SUMA_Alloc_DisplayObject_Struct(SUMA_MAX_DISPLAYABLE_OBJECTS);
    
    if (!SUMA_AllocSpecFields(spec)) { /* ZSS Jan 9 05 */
      fprintf( stderr, "** failed SUMA_AllocSpecFields(), exiting...\n" );
	   RETURN(-1);
    }
      
    if ( SUMA_Read_SpecFile( opts->spec_file, spec) == 0 )
    {
	fprintf( stderr, "** failed SUMA_Read_SpecFile(), exiting...\n" );
	RETURN(-1);
    }

    if ( opts->debug > 2 )
	SUMA_ShowSpecStruct(spec, stderr, 3);

    rv = SUMA_spec_select_surfs(spec,opts->surf_names,ST_MAX_SURFS,opts->debug);
    if ( rv < 1 )
    {
	if ( rv == 0 )
	    fprintf(stderr,"** no given surfaces found in spec file\n");
	RETURN(-1);
    }

    if ( opts->debug > 1 )
	SUMA_ShowSpecStruct(spec, stderr, opts->debug > 2 ? 3 : 1);

    if ( SUMA_spec_set_map_refs(spec, opts->debug) != 0 )
	RETURN(-1);

    /* make sure only group was read from spec file */
    if ( spec->N_Groups != 1 )
    {
	fprintf( stderr,"** error: N_Groups <%d> must be 1 in spec file <%s>\n",
		 spec->N_Groups, opts->spec_file );
	RETURN(-1);
    }

    /* actually load the surface(s) from the spec file */
    if (SUMA_LoadSpec_eng(spec, SUMAg_DOv, &SUMAg_N_DOv, opts->sv_file,
	                  opts->debug>3, SUMAg_CF->DsetList) == 0)
    {
	fprintf( stderr, "** error: failed SUMA_LoadSpec(), exiting...\n" );
	RETURN(-1);
    }

    if ( opts->debug > 0 )
	fputs( "++ surfaces loaded.\n", stderr );

    RETURN(0);
}


/*----------------------------------------------------------------------
 * add_to_flist			- add function name and code to list
 *----------------------------------------------------------------------
*/
int add_to_flist( func_t * F, char * fname )
{
ENTRY("add_to_flist");

    if ( F->nused >= F->nalloc )
    {
	F->nalloc += ST_DEFAULT_FALLOC;
	F->names   = (char **)realloc(F->names, F->nalloc*sizeof(char *));
	F->codes   = (int   *)realloc(F->codes, F->nalloc*sizeof(int   ));

	if ( !F->names || !F->codes )
	{
	    fprintf(stderr,"** failed to allocate for %d functs (%p,%p)\n",
		    F->nalloc, F->names, F->codes);
	    RETURN(-1);
	}
    }

    F->names[F->nused] = fname;
    F->codes[F->nused] = check_func_name(fname);

    if ( F->codes[F->nused] == E_SM_INVALID )
    {
	fprintf(stderr,"** function '%s' is not valid\n",
		CHECK_NULL_STR(fname));
	RETURN(-1);
    }

    F->nused++;			/* we've got another good one */

    RETURN(0);
}

int add_all_to_flist( func_t * F) 
{
   int c;
   
ENTRY("add_all_to_flist");

   if ( F->nused > 1 || F->codes[0] != E_SM_NODES) {
      fprintf(stderr,"** You should not use -func ALL with any other -func\n");
      RETURN(-1);   
   }
   F->nused = 0;
	for ( c = E_SM_INVALID + 1; c < E_SM_FINAL; c++ ) {
	    if (add_to_flist ( F, g_sm_names[c] ) < 0) {
         fprintf(stderr,"** Failed to add function %s\n", g_sm_names[c]);
         RETURN(-1);   
       }
   }
   
   RETURN(0);   
}

/*----------------------------------------------------------------------
 * init_opts_t			- initialize the struct
 *----------------------------------------------------------------------
*/
int init_opts_t( opts_t * opts )
{
    int c;

ENTRY("init_opts_t");

    memset(opts, 0, sizeof(opts_t));

    /* init the function list func_t struct */
    opts->F.names  = NULL;
    opts->F.codes  = NULL;
    opts->F.nalloc = 0;
    opts->F.nused  = 0;

    /* just to try this out, init the info list */
    if ( add_to_flist(&opts->F, g_sm_names[E_SM_NODES]) != 0 )
    {
	fprintf(stderr,"** failed to init func_t list with '%s'\n",
		g_sm_names[E_SM_NODES]);
	RETURN(-1);
    }

    opts->spec_file     = NULL;
    opts->sv_file       = NULL;
    opts->out_1D_file   = NULL;
    opts->cmask_cmd     = NULL;
    opts->nodes_1D_file = NULL;

    for ( c = 0; c < ST_MAX_SURFS; c++ )
	opts->surf_names[c] = NULL;

    opts->dnode         = -1;		/* init to something invalid */

    opts->out_file = NULL;
    RETURN(0);
}


/*----------------------------------------------------------------------*/
/* this macro is specifically for init_options(), below                 */

#define CHECK_ARG_COUNT(ac,str)     \
	do {                        \
            if ((ac+1) >= argc) {   \
		fputs(str,stderr);  \
		RETURN(-1);         \
	    }                       \
	} while (0)

/*----------------------------------------------------------------------
 * init_options
 *----------------------------------------------------------------------
*/
int init_options( opts_t * opts, int argc, char * argv[] )
{
    int ac, ind;

ENTRY("init_options");

    if ( argc < 2 )
	RETURN( usage(PROG_NAME, ST_USE_LONG) );

    /* init the structure to empty */
    if ( init_opts_t(opts) != 0 )
	RETURN(-1);

    for ( ac = 1; ac < argc; ac++ )
    {
	if ( ! strncmp(argv[ac], "-debug", 6) )
	{
	    CHECK_ARG_COUNT(ac,"option usage: -debug LEVEL\n");

	    opts->debug = atoi(argv[++ac]);
	    if ( opts->debug < 0 || opts->debug > ST_DEBUG_MAX_LEVEL )
	    {
		fprintf(stderr,"** bad debug level %d, should be in [0,%d]\n",
			opts->debug, ST_DEBUG_MAX_LEVEL);
		RETURN(-1);
	    }
	}
	else if ( ! strncmp(argv[ac], "-cmask", 6) )
	{
	    CHECK_ARG_COUNT(ac,"option usage: -cmask COMMAND\n");
	    opts->cmask_cmd = argv[++ac];
	}
	else if ( ! strncmp(argv[ac], "-dnode", 6) )
	{
	    CHECK_ARG_COUNT(ac,"option usage: -dnode NODE_NUM\n");
	    opts->dnode = atoi(argv[++ac]);
	}
	else if ( ! strncmp(argv[ac], "-func", 5) )
	{
	    CHECK_ARG_COUNT(ac,"option usage: -func FUNCTION\n");
	    ++ac;
	    if (!strcmp(argv[ac],"ALL")) {
         if (add_all_to_flist(&opts->F) != 0) RETURN(-1);
       } else {
         if ( add_to_flist(&opts->F, argv[ac]) != 0 ) RETURN(-1);
	    }
   }
	else if ( ! strncmp(argv[ac], "-help", 5) )
	    RETURN(usage(PROG_NAME, ST_USE_LONG));
	else if ( ! strncmp(argv[ac], "-hist", 5) )
	    RETURN(usage(PROG_NAME, ST_USE_HIST));
	else if ( ! strncmp(argv[ac], "-info_all",9) )
	{
	    opts->info |= ST_INFO_ALL;
	}
	else if ( ! strncmp(argv[ac], "-info_area",10) )
	{
	    opts->info |= ST_INFO_AREA;
	}
	else if ( ! strncmp(argv[ac], "-info_norms",10) )
	{
	    opts->info |= ST_INFO_NORMS;
	}
	else if ( ! strncmp(argv[ac], "-info_thick",11) )
	{
	    opts->info |= ST_INFO_THICK;
	}
	else if ( ! strncmp(argv[ac], "-info_vol",9) )
	{
	    opts->info |= ST_INFO_VOL;
	}
	else if ( ! strncmp(argv[ac], "-info_volg",9) )
	{
	    opts->info |= ST_INFO_VOLG;
	}
	else if ( ! strncmp(argv[ac], "-nodes_1D", 9) )
	{
	    CHECK_ARG_COUNT(ac,"option usage: -nodes_1D NODE_LIST_FILE\n");
	    opts->nodes_1D_file = argv[++ac];
	}
	else if ( ! strncmp(argv[ac], "-out_1D", 7) )
	{
	    CHECK_ARG_COUNT(ac,"option usage: -out_1D OUTPUT_FILE\n");
	    opts->out_1D_file = argv[++ac];
	}
   else if ( ! strcmp(argv[ac], "-out") )
	{
	    CHECK_ARG_COUNT(ac,"option usage: -out OUTPUT_DSET\n");
	    opts->out_file = argv[++ac];
	}
	else if ( ! strncmp(argv[ac], "-spec", 5) )
	{
	    CHECK_ARG_COUNT(ac,"option usage: -spec SPEC_FILE\n");
	    opts->spec_file = argv[++ac];
	}
	else if ( ! strncmp(argv[ac], "-surf_", 6) )
	{
	    CHECK_ARG_COUNT(ac,"option usage: -surf_X SURF_NAME\n");
	    ind = argv[ac][6] - 'A';
	    if ( (ind < 0) || (ind >= ST_MAX_SURFS) )
	    {
		fprintf(stderr,"** -surf_X option '%s' out of range,\n"
			"   use one of '-surf_A' through '-surf_%c'\n",
			argv[ac], 'A'+ST_MAX_SURFS-1);
		RETURN(-1);
	    }
	    opts->surf_names[ind] = argv[++ac];
	}
	else if ( ! strncmp(argv[ac], "-sv", 3) )
	{
	    CHECK_ARG_COUNT(ac,"option usage: -sv SURF_VOLUME\n");
	    opts->sv_file = argv[++ac];
	}
	else if ( ! strncmp(argv[ac], "-ver",4) )
	{
	    RETURN( usage(PROG_NAME, ST_USE_VERSION) );
	}
	else
	{
	    fprintf(stderr,"invalid option <%s>\n",argv[ac]);
	    suggest_best_prog_option(argv[0], argv[ac]);
            usage(PROG_NAME, ST_USE_SHORT);
            RETURN(-1);
	}
    }

    RETURN(0);
}


/*----------------------------------------------------------------------
 * validate_options
 *----------------------------------------------------------------------
*/
int validate_options( opts_t * opts, param_t * p )
{
    int errs = 0;

ENTRY("validate_options");    

    if ( !opts || !p )
    {
	fprintf(stderr,"** vo: bad params (%p,%p)\n", opts, p);
	RETURN(-1);
    }

    if ( opts->F.nused <= 0 )
    {
	fprintf(stderr,"** must specify at least one '-func' option\n");
	errs++;
    }

    if ( ! opts->spec_file )
    {
	fprintf(stderr,"** missing argument: -spec\n");
	errs++;
    }

    if ( ! opts->surf_names[0] )
    {
	fprintf(stderr,"** missing argument -surf_A\n");
	errs++;
    }

    /* we don't necessarily need an sv_file ... do not check */

    /* verify output file, and open for writing */
    if ( ! opts->out_1D_file && ! opts->out_file)
    {
	fprintf(stderr,"** missing argument: -out_1D or -out\n");
	errs++;
    }
    else if ( opts->out_1D_file && 
              THD_is_file(opts->out_1D_file) && !THD_ok_overwrite())
    {
	fprintf(stderr,"** output file already exists: %s\n",opts->out_1D_file);
	errs++;
    } else if ( opts->out_file && 
                THD_is_file(opts->out_file) && !THD_ok_overwrite())
    {
	fprintf(stderr,"** output file already exists: %s\n",opts->out_file);
	errs++;
    } 

    
    if ( errs > 0 )
	RETURN(-1);

    if ( opts->debug > 1 )
    {
	disp_opts_t( "-- opts okay: ", opts );
	disp_func_t( "-- opts okay: ", &opts->F );
    }


    /* options look good, now fill the param_t struct */
    memset(p, 0, sizeof(param_t));	/* clear params    */

    p->S.slist    = NULL;			/* to be safe...   */
    p->S.narea[0] = NULL;
    p->S.narea[1] = NULL;
    p->S.nvol     = NULL;
    p->S.nvolg    = NULL;
    p->S.fvol     = NULL;

    p->F          = &opts->F;			/* point to struct */

    if ( opts->out_1D_file && 
         (p->outfp = fopen(opts->out_1D_file, "w")) == NULL )
    {
	fprintf(stderr,"** cannot open output file '%s'\n",opts->out_1D_file);
	RETURN(-1);
    }

    /* init before filling */
    p->nodes  = NULL;
    p->nnodes = 0;
    p->cmask  = NULL;
    p->ncmask = 0;
    p->out_dset = NULL;
    p->colmap = NULL;
    
    if ( opts->nodes_1D_file )
    {
	if ( read_nodes_file(opts, p) != 0 )
	    RETURN(-1);
    }

    if ( opts->cmask_cmd )
    {
	if ( get_cmask(opts, p) != 0 )
	    RETURN(-1);
    }

    RETURN(0);
}


/*----------------------------------------------------------------------
 * get_cmask				- get cmask from command
 *----------------------------------------------------------------------
*/
int get_cmask( opts_t * opts, param_t * p )
{
    char * cmd;
    int    clen;

ENTRY("get_cmask");

    if ( ! opts->cmask_cmd )
	RETURN(0);

    clen = strlen(opts->cmask_cmd);
    cmd  = (char *)malloc((clen + 1)*sizeof(char));
    ALLOC_CHECK(cmd, "char", clen);

    strcpy(cmd, opts->cmask_cmd);

    p->cmask = EDT_calcmask(cmd, &p->ncmask, p->nnodes);

    free(cmd);		/* we are done with the now corrupted command */

    if ( ! p->cmask || p->ncmask < 1 )
    {
	fprintf(stderr,"** failure: cannot compute cmask from option:\n"
		"   -cmask '%s'\n", opts->cmask_cmd);
	RETURN(-1);
    }

    p->ccount = THD_countmask( p->ncmask, p->cmask );

    if ( p->ccount < 1 )		/* do not quit */
	fprintf(stderr,"** warning: cmask is empty from option\n"
		"   -cmask '%s'\n", opts->cmask_cmd);

    if ( opts->debug > 0 )
	fprintf(stderr,"++ have cmask with %d of %d set entries\n",
		p->ccount, p->ncmask);

    RETURN(0);
}


/*----------------------------------------------------------------------
 * read_nodes_file			- read 1D file with node list
 *----------------------------------------------------------------------
*/
int read_nodes_file( opts_t * opts, param_t * p )
{
    MRI_IMAGE * im;
    float     * fim;
    char      * nfile;
    int         index;

ENTRY("read_nodes_file");

    nfile = opts->nodes_1D_file;		/* for ease of typing */

    if ( !nfile )
	RETURN(0);

    if ( (im = mri_read_1D(nfile)) == NULL )
    {
	fprintf(stderr,"** failed to read 1D nodes file '%s'\n", nfile);
	RETURN(-1);
    }
    
    if ( im->nx < 1 )
    {
	fprintf(stderr,"** node list file '%s' appears empty\n", nfile);
	RETURN(-1);
    }

    if ( im->ny > 1 )
    {
	fprintf(stderr,"** please specify only one column of node file '%s'\n"
		       "   e.g.  -nodes_1D 'surf_data.1D[0]'\n", nfile);
	RETURN(-1);
    }

    p->nnodes = im->nx;
    p->nodes  = (int *)malloc(im->nx * sizeof(int));
    ALLOC_CHECK(p->nodes, "int", im->nx);

    /* now get values */
    fim = MRI_FLOAT_PTR(im);
    for ( index = 0; index < p->nnodes; index++ )
	p->nodes[index] = (int)fim[index];

    if ( opts->debug > 0 )
	fprintf(stderr,"++ read %d node indices from '%s'\n", p->nnodes, nfile);

    mri_free(im);		/* we're done with image */

    RETURN(0);
}


/* so I don't have to repeat this all over (and can do it per function)... */

#define SM_2SURF_TEST(code)						    \
	    do{ if (p->S.nsurf<2) {					    \
		    fprintf(stderr,"** function %s requires 2 surfaces\n",  \
			    g_sm_names[fcodes[code]]);			    \
		    errs++; 						    \
	        }							    \
	    } while (0);

/*----------------------------------------------------------------------
 * validate_option_lists		- check function and info entries
 *----------------------------------------------------------------------
*/
int validate_option_lists( opts_t * opts, param_t * p )
{
    int * fcodes, c, errs;

ENTRY("validate_option_lists");

    errs = 0;

    if ( (opts->info & ST_INFO_THICK) && p->S.nsurf < 2 )
    {
	fprintf(stderr,"** -info_thick option requiers 2 surfaces\n");
	errs++;
    }

    if ( (opts->info & ST_INFO_VOL) && p->S.nsurf < 2 )
    {
	fprintf(stderr,"** -info_vol option requiers 2 surfaces\n");
	errs++;
    }

    if ( (opts->info & ST_INFO_VOLG) && p->S.nsurf < 2 )
    {
	fprintf(stderr,"** -info_volg option requiers 2 surfaces\n");
	errs++;
    }

    /* verify all the functions in our list */
    fcodes = p->F->codes;	/* just for less typing */
    for ( c = 0; c < p->F->nused; c++ )
    {
	switch (fcodes[c])
	{
	    default:
		fprintf(stderr, "** vol: invalid function code %d\n",fcodes[c]);
		errs++;
		break;

	    case E_SM_COORD_A:
	    case E_SM_N_AREA_A:
	    case E_SM_N_AVEAREA_A:
	    case E_SM_NODES:
	    case E_SM_NORM_A:
		break;

	    case E_SM_ANG_NS_A:
		if ( !p->S.slist[0]->NodeNormList )
		{
		    fprintf(stderr,"** missing node normals for func '%s'\n",
			    g_sm_names[fcodes[c]]);
		    errs++;
		}
		break;

	    case E_SM_ANG_NORMS:
	    case E_SM_ANG_NS_B:
	    case E_SM_NORM_B:
		if ( !p->S.slist[0]->NodeNormList ||
		     !p->S.slist[1]->NodeNormList )
		{
		    fprintf(stderr,"** missing node normals for func '%s'\n",
			    g_sm_names[fcodes[c]]);
		    errs++;
		}

		SM_2SURF_TEST(c);
		break;
		
	    case E_SM_COORD_B:
	    case E_SM_N_AREA_B:
	    case E_SM_N_AVEAREA_B:
	    case E_SM_NODE_VOL:
	    case E_SM_NODE_VOLG:
	    case E_SM_THICK:

		SM_2SURF_TEST(c);
		break;

	    case E_SM_NTRI:
		if ( ! p->S.slist[0]->MF->N_Memb )
		{
		    fprintf(stderr,"** missing Face Member list, func '%s'\n",
			    g_sm_names[fcodes[c]]);
		    errs++;
		}

		break;
	}

	if ( opts->debug > 1 && errs == 0 )
	    fprintf(stderr,"++ have valid function name '%s'\n",
		    g_sm_names[fcodes[c]]);
    }

    if ( opts->debug > 0 && errs > 0 )
	fprintf(stderr,"** validate_option_lists: %d errors found\n", errs);

    if ( errs > 0 )
	RETURN(-1);

    if ( opts->debug > 0 )
	fprintf(stderr,"-- found %d output functions\n", p->F->nused);

    RETURN(0);
}



/*----------------------------------------------------------------------
 * usage              - provide info, depending on the type
 *
 * ST_USE_SHORT
 * ST_USE_LONG
 * ST_USE_VERSION
 *
 * return non-zero to signal this as a terminal function
 *        1 on good return, -1 on error
 *----------------------------------------------------------------------
*/
int usage( char * prog, int use_type )
{
    int c, rval=1;  /* 1 means good return, -1 means error */

ENTRY("usage");

    if ( use_type == ST_USE_SHORT )
    {
	fprintf(stderr,"usage: %s [options] -spec SPEC_FILE -func FUNC_NAME\\\n"
	               "                    -out OUTFILE\n", prog);
    }
    else if ( use_type == ST_USE_LONG )
    {
	printf(
	    "\n"
	    "%s - compute measures from the surface dataset(s)\n"
	    "\n"
	    "  usage: %s [options] -spec SPEC_FILE -out OUTFILE\n"
	    "\n"
	    "    This program is meant to read in a surface or surface pair,\n"
	    "    and to output and user-requested measures over the surfaces.\n"
	    "    The surfaces must be specified in the SPEC_FILE.\n"
	    "\n"
	    " ** Use the 'inspec' command for getting information about the\n"
	    "    surfaces in a spec file.\n"
	    "\n"
	    "    The output will be a 1D format text file, with one column\n"
	    "    (or possibly 3) per user-specified measure function.  Some\n"
	    "    functions require only 1 surface, some require 2.\n"
	    "\n"
	    "    Current functions (applied with '-func') include:\n"
	    "\n",
	    prog, prog );

	/* display current list of measures */
	for ( c = E_SM_INVALID + 1; c < E_SM_FINAL; c++ )
	    printf( "        %-12s : %s\n", g_sm_names[c], g_sm_desc[c]);

	printf(
	    "\n"
	    "------------------------------------------------------------\n"
	    "\n"
	    "  examples:\n"
	    "\n"
	    "    1. For each node on the surface smoothwm in the spec file,\n"
	    "       fred.spec, output the node number (the default action),\n"
	    "       the xyz coordinates, and the area associated with the\n"
	    "       node (1/3 of the total area of triangles having that node\n"
	    "       as a vertex).\n"
	    "\n"
	    "        %s                                   \\\n"
	    "            -spec       fred1.spec                     \\\n"
	    "            -sv         fred_anat+orig                 \\\n"
	    "            -surf_A     smoothwm                       \\\n"
	    "            -func       coord_A                        \\\n"
	    "            -func       n_area_A                       \\\n"
	    "            -out_1D     fred1_areas.1D                   \n"
	    "\n"
	    "    2. For each node of the surface pair smoothwm and pial,\n"
	    "       display the:\n"
	    "         o  node index\n"
	    "         o  node's area from the first surface\n"
	    "         o  node's area from the second surface\n"
	    "         o  node's resulting volume\n"
	    "         o  thickness at that node (segment distance)\n"
	    "         o  coordinates of the first segment node\n"
	    "         o  coordinates of the second segment node\n"
	    "\n"
	    "         Additionally, display total surface areas, minimum and\n"
	    "         maximum thicknesses, and total volume for the\n"
	    "         cortical ribbon (the sum of node volumes).\n"
	    "\n"
	    "        %s                                   \\\n"
	    "            -spec       fred2.spec                     \\\n"
	    "            -sv         fred_anat+orig                 \\\n"
	    "            -surf_A     smoothwm                       \\\n"
	    "            -surf_B     pial                           \\\n"
	    "            -func       n_area_A                       \\\n"
	    "            -func       n_area_B                       \\\n"
	    "            -func       node_volg                      \\\n"
	    "            -func       thick                          \\\n"
	    "            -func       coord_A                        \\\n"
	    "            -func       coord_B                        \\\n"
	    "            -info_area                                 \\\n"
	    "            -info_thick                                \\\n"
	    "            -info_vol                                  \\\n"
	    "            -out        fred2_vol.niml.dset              \n"
	    "\n"
	    "    3. For each node of the surface pair, display the:\n"
	    "         o  node index\n"
	    "         o  angular diff between the first and second norms\n"
	    "         o  angular diff between the segment and first norm\n"
	    "         o  angular diff between the segment and second norm\n"
	    "         o  the normal vectors for the first surface nodes\n"
	    "         o  the normal vectors for the second surface nodes\n"
	    "         o  angular diff between the segment and second norm\n"
	    "\n"
	    "        %s                                   \\\n"
	    "            -spec       fred2.spec                     \\\n"
	    "            -surf_A     smoothwm                       \\\n"
	    "            -surf_B     pial                           \\\n"
	    "            -func       ang_norms                      \\\n"
	    "            -func       ang_ns_A                       \\\n"
	    "            -func       ang_ns_B                       \\\n"
	    "            -func       norm_A                         \\\n"
	    "            -func       norm_B                         \\\n"
	    "            -out        fred2_norm_angles                \n"
	    "\n"
	    "    4. Similar to #3, but output extra debug info, and in\n"
	    "       particular, info regarding node 5000.\n"
	    "\n"
	    "        %s                                   \\\n"
	    "            -spec       fred2.spec                     \\\n"
	    "            -sv         fred_anat+orig                 \\\n"
	    "            -surf_A     smoothwm                       \\\n"
	    "            -surf_B     pial                           \\\n"
	    "            -func       ang_norms                      \\\n"
	    "            -func       ang_ns_A                       \\\n"
	    "            -func       ang_ns_B                       \\\n"
	    "            -debug      2                              \\\n"
	    "            -dnode      5000                           \\\n"
	    "            -out        fred2_norm_angles.1D             \n"
	    "\n"
	    "    5. For each node, output the  volume, thickness\n"
	    "       and areas, but restrict the nodes to the list contained in\n"
	    "       column 0 of file sdata.1D.  Furthermore, restrict those \n"
	    "       nodes to the mask inferred by the given '-cmask' option.\n"
	    "\n"
	    "        %s                                                   \\\n"
	    "            -spec       fred2.spec                           \\\n"
	    "            -sv         fred_anat+orig                       \\\n"
	    "            -surf_A     smoothwm                             \\\n"
	    "            -surf_B     pial                                 \\\n"
	    "            -func       node_volg                            \\\n"
	    "            -func       thick                                \\\n"
	    "            -func       n_area_A                             \\\n"
	    "            -func       n_area_B                             \\\n"
	    "            -nodes_1D   'sdata.1D[0]'                        \\\n"
	    "            -cmask      '-a sdata.1D[2] -expr step(a-1000)'  \\\n"
	    "            -out        fred2_masked.1D                        \n"
	    "\n"
	    "------------------------------------------------------------\n"
	    "\n"
	    "  REQUIRED COMMAND ARGUMENTS:\n"
	    "\n"
	    "    -spec SPEC_FILE       : SUMA spec file\n"
	    "\n"
	    "        e.g. -spec fred2.spec\n"
	    "\n"
	    "        The surface specification file contains a list of\n"
	    "        related surfaces.  In order for a surface to be\n"
	    "        processed by this program, it must exist in the spec\n"
	    "        file.\n"
	    "\n"
	    "    -surf_A SURF_NAME     : surface name (in spec file)\n"
	    "    -surf_B SURF_NAME     : surface name (in spec file)\n"
	    "\n"
	    "        e.g. -surf_A smoothwm\n"
	    "        e.g. -surf_A lh.smoothwm\n"
	    "        e.g. -surf_B lh.pial\n"
	    "\n"
	    "        This is used to specify which surface(s) will be used\n"
	    "        by the program.  The 'A' and 'B' correspond to other\n"
	    "        program options (e.g. the 'A' in n_area_A).\n"
	    "\n"
	    "        The '-surf_B' parameter is required only when the user\n"
	    "        wishes to input two surfaces.\n"
	    "\n"
	    "        Any surface name provided must be unique in the spec\n"
	    "        file, and must match the name of the surface data file\n"
	    "        (e.g. lh.smoothwm.asc).\n"
	    "\n"
	    "    -out_1D OUT_FILE.1D   : 1D output filename\n"
	    "\n"
	    "        e.g. -out_1D pickle_norm_info.1D\n"
	    "\n"
	    "        This option is used to specify the name of the output\n"
	    "        file.  The output file will be in the 1D ascii format,\n"
	    "        with 2 rows of comments for column headers, and 1 row\n"
	    "        for each node index.\n"
	    "\n"
	    "        There will be 1 or 3 columns per '-func' option, with\n"
	    "        a default of 1 for \"nodes\".\n"
       "\n"
       "        Consider using the newer -out instead of -out_1D\n"
	    "\n"
       "\n"
	    "    -out OUT_DSET   : Output into surface dataset OUT_DSET\n"
	    "\n"
	    "        e.g. -out pickle_norm_info.niml.dset\n"
	    "\n"
       "        The dset format is determined from the extension of\n"
       "        OUT_DSET. Default is NIML format.\n"
       "        You are better off using -out and non-1D format datasets\n"
       "        because non-1D datasets are better handled by 3dcalc\n"
       "\n"
       "        You can use both -out and -out_1D, but why would you do this?\n"
       "\n"
	    "------------------------------------------------------------\n"
	    "\n"
	    "  ALPHABETICAL LISTING OF OPTIONS:\n"
	    "\n"
	    "    -cmask COMMAND        : restrict nodes with a mask\n"
	    "\n"
	    "        e.g.     -cmask '-a sdata.1D[2] -expr step(a-1000)'\n"
	    "\n"
	    "        This option will produce a mask to be applied to the\n"
	    "        list of surface nodes.  The total mask size, including\n"
	    "        zero entries, must match the number of nodes.  If a\n"
	    "        specific node list is provided via the '-nodes_1D'\n"
	    "        option, then the mask size should match the length of\n"
	    "        the provided node list.\n"
	    "        \n"
	    "        Consider the provided example using the file sdata.1D.\n"
	    "        If a surface has 100000 nodes (and no '-nodes_1D' option\n"
	    "        is used), then there must be 100000 values in column 2\n"
	    "        of the file sdata.1D.\n"
	    "\n"
	    "        Alternately, if the '-nodes_1D' option is used, giving\n"
	    "        a list of 42 nodes, then the mask length should also be\n"
	    "        42 (regardless of 0 entries).\n"
	    "\n"
	    "        See '-nodes_1D' for more information.\n"
	    "\n"
	    "    -debug LEVEL          : display extra run-time info\n"
	    "\n"
	    "        e.g.     -debug 2\n"
	    "        default: -debug 0\n"
	    "\n"
	    "        Valid debug levels are from 0 to 5.\n"
	    "\n"
	    "    -dnode NODE           : display extra info for node NODE\n"
	    "\n"
	    "        e.g. -dnode 5000\n"
	    "\n"
	    "        This option can be used to display extra information\n"
	    "        about node NODE during surface evaluation.\n"
	    "\n"
	    "    -func FUNCTION        : request output for FUNCTION\n"
	    "\n"
	    "        e.g. -func thick\n"
	    "\n"
	    "        This option is used to request output for the given\n"
	    "        FUNCTION (measure).  Some measures produce one column\n"
	    "        of output (e.g. thick or ang_norms), and some produce\n"
	    "        three (e.g. coord_A).  These options, in the order they\n"
	    "        are given, determine the structure of the output file.\n"
	    "\n"
	    "        Current functions include:\n"
	    "\n",
	    prog, prog, prog, prog, prog );

	/* display current list of measures */
	for ( c = E_SM_INVALID + 1; c < E_SM_FINAL; c++ )
	    printf( "            %-12s : %s\n", g_sm_names[c], g_sm_desc[c]);

	printf(
	    "\n"
	    "          Note that with node_vol, the node volumes can be a little\n"
       "          biased. It is recommended you use -node_volg instead.\n" 
	    /*"          Places where either normal points in the 'wrong' direction\n"
	    "          will be incorrect, as will be the parts of the surface\n"
	    "          that 'encompass' this region.  Maybe we could refer\n"
	    "          to this as a mushroom effect...\n"
	    "\n"
	    "          Basically, expect the total volume to be around 10%%\n"
	    "          too large.\n"
	    "\n"
	    "          ** for more accuracy, try 'SurfPatch -vol' **\n"*/
	    "\n"
       "          You can also use -func ALL to get everything output.\n"
       "          You should not use other -func options with -func ALL\n" 
       "\n"
	    "    -help                 : show this help menu\n"
	    "\n"
	    "    -hist                 : display program revision history\n"
	    "\n"
	    "        This option is used to provide a history of changes\n"
	    "        to the program, along with version numbers.\n"
	    "\n"
	    "  NOTE: the following '-info_XXXX' options are used to display\n"
	    "        pieces of 'aggregate' information about the surface(s).\n"
	    "\n"
	    "    -info_all             : display all final info\n"
	    "\n"
	    "        This is a short-cut to get all '-info_XXXX' options.\n"
	    "\n"
	    "    -info_area            : display info on surface area(s)\n"
	    "\n"
	    "        Display the total area of each triangulated surface.\n"
	    "\n"
	    "    -info_norms           : display info about the normals\n"
	    "\n"
	    "        For 1 or 2 surfaces, this will give (if possible) the\n"
	    "        average angular difference between:\n"
	    "\n"
	    "            o the normals of the surfaces\n"
	    "            o the connecting segment and the first normal\n"
	    "            o the connecting segment and the second normal\n"
	    "\n"
	    "    -info_thick           : display min and max thickness\n"
	    "\n"
	    "        For 2 surfaces, this is used to display the minimum and\n"
	    "        maximum distances between the surfaces, along each of\n"
	    "        the connecting segments.\n"
	    "\n"
	    "    -info_vol             : display info about the volume\n"
	    "\n"
	    "        For 2 surfaces, display the total computed volume.\n"
	    "        Note that this node-wise volume computation is an\n"
            "        approximation, and tends to run ~10 %% high.\n"
	    "\n"
	    "        ** for more accuracy, use -info_volg **\n"
       "\n"
	    "    -info_volg             : display info about the volume\n"
	    "                             which is estimated with Gauss'\n"
       "                             theorem.\n"
	    "\n"
	    "    -nodes_1D NODELIST.1D : request output for only these nodes\n"
	    "\n"
	    "        e.g.  -nodes_1D node_index_list.1D\n"
	    "        e.g.  -nodes_1D sdata.1D'[0]'\n"
	    "\n"
	    "        The NODELIST file should contain a list of node indices.\n"
	    "        Output from the program would then be restricted to the\n"
	    "        nodes in the list.\n"
	    "        \n"
	    "        For instance, suppose that the file BA_04.1D contains\n"
	    "        a list of surface nodes that are located in Broadman's\n"
	    "        Area 4.  To get output from the nodes in that area, use:\n"
	    "        \n"
	    "            -nodes_1D BA_04.1D\n"
	    "        \n"
	    "        For another example, suppose that the file sdata.1D has\n"
	    "        node indices in column 0, and Broadman's Area indices in\n"
	    "        column 3.  To restrict output to the nodes in Broadman's\n"
	    "        area 4, use the pair of options:\n"
	    "        \n"
	    "            -nodes_1D 'sdata.1D[0]'                     \\\n"
	    "            -cmask '-a sdata.1D[3] -expr (1-bool(a-4))' \n"
	    "\n"
	    "    -sv SURF_VOLUME       : specify an associated AFNI volume\n"
	    "\n"
	    "        e.g. -sv fred_anat+orig\n"
	    "\n"
	    "        If there is any need to know the orientation of the\n"
	    "        surface, a surface volume dataset may be provided.\n"
	    "\n"
	    "    -ver                  : show version information\n"
	    "\n"
	    "        Show version and compile date.\n"
	    "\n"
	    "------------------------------------------------------------\n"
	    "\n"
	    "  Author: R. Reynolds  - %s\n"
	    "\n",
	    VERSION );
    }
    else if ( use_type == ST_USE_HIST )
    {
	fputs (g_history, stdout);
    }
    else if ( use_type == ST_USE_VERSION )
    {
	printf("%s: %s, compile date: %s\n", prog, VERSION, __DATE__);
    }
    else {
	fprintf(stderr,"** error: usage - invalid use_type %d\n", use_type); 
        rval = -1;
    }

    RETURN(rval);
}


/*----------------------------------------------------------------------
 * check_func_name			- return function code for name
 *----------------------------------------------------------------------
*/
int check_func_name( char * func )
{
    int fnum;

ENTRY("check_func_name");

    if ( !func )
	RETURN(E_SM_INVALID);

    /* just to be safe, let's verify the names and enum */

    if ( sizeof(g_sm_names)/sizeof(char *) != (int)E_SM_FINAL )
    {
	fprintf(stderr,"** error: g_sm_names mis-match\n");
	RETURN(E_SM_INVALID);
    }

    if ( sizeof(g_sm_desc)/sizeof(char *) != (int)E_SM_FINAL )
    {
	fprintf(stderr,"** error: g_sm_desc mis-match\n");
	RETURN(E_SM_INVALID);
    }

    for ( fnum = E_SM_INVALID; fnum < E_SM_FINAL; fnum++ )
	if ( !strcmp(func, g_sm_names[fnum]) )
	    RETURN(fnum);

    RETURN(E_SM_INVALID);
}


/*----------------------------------------------------------------------
 * final_cleanup			- free memory
 *----------------------------------------------------------------------
*/
int final_cleanup( opts_t * opts, param_t * p )
{

ENTRY("final_cleanup");

    /* first, close the output file, the rest are in order */
    if ( p->outfp && p->outfp != stdout )
	   fclose(p->outfp);
    SUMA_FreeSpecFields(&(p->S.spec)); /* ZSS Jan 9 06 */
    if ( p->S.narea[0] )  free(p->S.narea[0]);
    if ( p->S.narea[1] )  free(p->S.narea[1]);
    if ( p->S.slist    )  free(p->S.slist);
    if ( p->S.nvol     )  free(p->S.nvol);
    if ( p->S.nvolg    )  free(p->S.nvolg);
    if ( p->S.fvol     )  free(p->S.fvol);

    if ( p->F->nalloc > 0 )
    {
	free(p->F->names);
	free(p->F->codes);
    }

    if ( p->nodes )
	free(p->nodes);

    if ( p->cmask )
	free(p->cmask);

    if ( opts->debug > 2 )
	fprintf(stderr,"-- freeing SUMA data...\n");

    if ( ( SUMAg_DOv != NULL ) &&
	 ( SUMA_Free_Displayable_Object_Vect(SUMAg_DOv, SUMAg_N_DOv) == 0 ) )
	fprintf(stderr, "** failed SUMA_Free_Displayable_Object_Vect()\n" );

    if ( ( SUMAg_SVv != NULL ) &&
	 ( SUMA_Free_SurfaceViewer_Struct_Vect(SUMAg_SVv, SUMAg_N_SVv) == 0 ) )
	fprintf( stderr, "** failed SUMA_Free_SurfaceViewer_Struct_Vect()\n" );

    if ( ( SUMAg_CF != NULL ) && ( SUMA_Free_CommonFields(SUMAg_CF) == 0 ) )
	fprintf( stderr, "** failed SUMA_Free_CommonFields()\n" );

    RETURN(0);
}


/*----------------------------------------------------------------------
 * tetra_volume			- retrun the volume of the tetrahedron
 *
 * 	V = 1/6 * |a o (b x c)|               -- mathworld.wolfram.com
 *----------------------------------------------------------------------
*/
double tetra_volume( float * p0, float * p1, float * p2, float * p3 )
{
    double v0[3], v1[3], v2[3], xp[3];

    v0[0] = p1[0] - p0[0];
    v0[1] = p1[1] - p0[1];
    v0[2] = p1[2] - p0[2];

    v1[0] = p2[0] - p0[0];
    v1[1] = p2[1] - p0[1];
    v1[2] = p2[2] - p0[2];

    v2[0] = p3[0] - p0[0];
    v2[1] = p3[1] - p0[1];
    v2[2] = p3[2] - p0[2];

    cross_product(xp, v1, v2);

    return(fabs(dot_product(v0,xp))/6.0);
}


/*----------------------------------------------------------------------
 * dist_fn            - return Euclidean distance between the points
 *----------------------------------------------------------------------
*/
double dist_fn( int len, float * p1, float * p2 )
{
    double diff, sum;
    int    c;

    if ( len < 1 || p1 == NULL || p2 == NULL )
    {
	fprintf(stderr, "** dist_fn: invalid params (%d,%p,%p)\n",
		len, p1, p2);
	return 0.0;
    }

    sum = 0.0;
    for ( c = 0; c < len; c++ )
    {
	diff = p1[c] - p2[c];
	sum += diff * diff;
    }

    return sqrt(sum);
}


/*----------------------------------------------------------------------
 * disp_surf_t
 *----------------------------------------------------------------------
*/
int disp_surf_t( char * info, surf_t * d )
{
    if ( info )
	fputs( info, stderr );

    if ( ! d )
    {
	fprintf(stderr,"** disp_surf_t: d == NULL\n");
	return -1;
    }

    fprintf(stderr,
	    "surf_t struct at %p:\n"
	    "    spec N_Surfs      = %d\n"
	    "    spec N_Groups     = %d\n"
	    "    slist             = %p\n"
	    "    narea[0,1]        = %p, %p\n"
	    "    nvol, nvolg, fvol = %p, %p, %p\n"
	    "    nsurf, salloc     = %d, %d\n"
	    "    nnodes, nfaces    = %d, %d\n",
	    d,
	    d->spec.N_Surfs, d->spec.N_Groups, d->slist,
	    d->narea[0], d->narea[1], d->nvol, d->nvolg, d->fvol,
	    d->nsurf, d->salloc, d->nnodes, d->nfaces);

    return 0;
}


/*----------------------------------------------------------------------
 * disp_func_t
 *----------------------------------------------------------------------
*/
int disp_func_t( char * info, func_t * d )
{
    if ( info )
	fputs( info, stderr );

    if ( ! d )
    {
	fprintf(stderr,"** disp_func_t: d == NULL\n");
	return -1;
    }

    fprintf(stderr,
	    "func_t struct at %p:\n"
	    "    names, codes   = %p, %p\n"
	    "    nalloc, nused  = %d, %d\n",
	    d,
	    d->names, d->codes, d->nalloc, d->nused);

    return 0;
}


/*----------------------------------------------------------------------
 * disp_opts_t
 *----------------------------------------------------------------------
*/
int disp_opts_t( char * info, opts_t * d )
{
    if ( info )
	fputs( info, stderr );

    if ( ! d )
    {
	fprintf(stderr,"** disp_opts_t: d == NULL\n");
	return -1;
    }

    fprintf(stderr,
	    "opts_t struct at %p:\n"
	    "    spec_file       = %s\n"
	    "    sv_file         = %s\n"
	    "    out_1D_file     = %s\n"
	    "    cmask_cmd       = %s\n"
	    "    nodes_1D_file   = %s\n"
	    "    surf_names[0,1] = %s, %s\n"
	    "    info            = %0x\n"
	    "    debug, dnode    = %d, %d\n",
	    d,
	    CHECK_NULL_STR(d->spec_file),
	    CHECK_NULL_STR(d->sv_file),
	    CHECK_NULL_STR(d->out_1D_file),
	    CHECK_NULL_STR(d->cmask_cmd),
	    CHECK_NULL_STR(d->nodes_1D_file),
	    CHECK_NULL_STR(d->surf_names[0]),
	    CHECK_NULL_STR(d->surf_names[1]),
	    d->info, d->debug, d->dnode);

    return 0;
}


/*----------------------------------------------------------------------
 * disp_param_t
 *----------------------------------------------------------------------
*/
int disp_param_t( char * info, param_t * d )
{
    if ( info )
	fputs( info, stderr );

    if ( ! d )
    {
	fprintf(stderr,"** disp_param_t: d == NULL\n");
	return -1;
    }

    fprintf(stderr,
	    "param_t struct at %p:\n"
	    "    F, outfp       = %p, %p\n"
	    "    nodes, nnodes  = %p, %d\n"
	    "    cmask          = %p\n"
	    "    ncmask, ccount = %d, %d\n",
	    d,
	    d->F, d->outfp, d->nodes, d->nnodes,
	    d->cmask, d->ncmask, d->ccount );

    return 0;
}


/*----------------------------------------------------------------------
 * disp_f3_point
 *----------------------------------------------------------------------
*/
int disp_f3_point( char * info, float * d )
{
    if ( info )
	fputs( info, stderr );

    if ( ! d )
    {
	fprintf(stderr,"** disp_f3_point: d == NULL\n");
	return -1;
    }

    fprintf(stderr,"(%6s, ", MV_format_fval(d[0]));
    fprintf(stderr,"%6s, ",  MV_format_fval(d[1]));
    fprintf(stderr,"%6s)\n", MV_format_fval(d[2]));

    return 0;
}


/*----------------------------------------------------------------------
 * lazy_det			- compute determinant
 * 				- assume small, don't search for efficiency
 *----------------------------------------------------------------------
*/
double lazy_det( int width, double * data )
{
    static int   total_size = 0;
    double     * newm, * rowp, * cur, rv;
    int          c, row, col, w2, sign;

    if ( width < 2 )
	return *data;

    if ( width == 2 )
	return( data[0]*data[3] - data[1]*data[2] );

    /* otherwise, we need more space */
    w2 = width - 1;
    total_size += w2*w2;
    newm = malloc(w2*w2 * sizeof(double));
    if ( !newm )
    {
	fprintf(stderr,"** failed to allocate %d doubles for mat (%d ttl)\n",
		w2*w2, total_size);
	return 0.0;
    }

    rv   = 0.0;
    sign = -1;
    for ( c = 0; c < width; c++ )
    {
	sign = -sign;				/* catch the continue */

	if ( data[c] == 0.0 )			/* skip any zeros */
	    continue;

	/* start by copying new data */
	for ( row = 1; row < width; row++ )	/* always skip row 0 */
	{
	    rowp = data + row     * width;	/* point to source column */
	    cur  = newm + (row-1) * w2;		/* init dest pointer */

	    for ( col = 0; col < width; col++ )
	    {
		if ( col == c )
		    continue;

		*cur++ = rowp[col];
	    }
	}

	rv += sign * data[c] * lazy_det( w2, newm );
    }

    free(newm);

    return rv;
}


