#define MAIN

#include "mrilib.h"
#include "afni.h"
#include <stdio.h>
#include <stdlib.h>
#include "thd_ttatlas_query.h"
#include "rickr/r_new_resam_dset.h"

#define SEG_UNKNOWN  0
#define SEG_CSF      1
#define SEG_GM       2
#define SEG_WM       4

const char *SegValToSegName(int s)
{
   switch (s) {
      case SEG_UNKNOWN:
         RETURN("Unknown");
      case SEG_CSF:
         RETURN("CSF");
      case SEG_WM:
         RETURN("WM");
      case SEG_GM:
         RETURN("GM");
      default:
         RETURN("Smoking?");
   }
}

#define SEG_METH_UNKNOWN   0
#define SEG_METH_DIST      1
#define SEG_METH_T         2

const char *SegMethToMethName(int s)
{
   switch (s) {
      case SEG_METH_UNKNOWN:
         RETURN("Unknown");
      case SEG_METH_DIST:
         RETURN("Distance");
      case SEG_METH_T:
         RETURN("T");
      default:
         RETURN("Smoking?");
   }
}

static int vn=0 ;

static void vstep_print(void)
{
   static char xx[10] = "0123456789" ;
   fprintf(stderr , "%c" , xx[vn%10] ) ;
   if( vn%10 == 9) fprintf(stderr,".") ;
   vn++ ;
}

void Segment_usage(void) 
{
   int i = 0;
   
   ENTRY("Segment_usage");
   
   printf(  "Usage: Segment -anat ANAT -csf CSF -wm WM -gm GM -mask mset\n"
               ".\n"
               "-debug DEBUG\n"
               "-prefix PREFIX\n"
               "-sphere_hood RAD  default is 4.0\n" 
               "-T_met\n"
               "-Dist_met\n"
               "-max_loops\n"
               "\n");
   EXRETURN;
}

typedef struct {
   char *aset_name;
   char *mset_name;
   char *csfinit_name;
   char *wminit_name;
   char *gminit_name;
   char *prefix;
   THD_3dim_dataset *aset;
   THD_3dim_dataset *mset;
   THD_3dim_dataset *csfinit;
   THD_3dim_dataset *wminit;
   THD_3dim_dataset *gminit;
   THD_3dim_dataset *oset;
   int debug;
   int idbg, jdbg, kdbg;
   float na;
   int DistMetric;
   int N_loopmax;
} SEG_OPTS;

SEG_OPTS *Segment_ParseInput (char *argv[], int argc)
{
   static char FuncName[]={"Segment_ParseInput"}; 
   SEG_OPTS *Opt=NULL;
   int kar, i, ind, exists;
   char *outname, cview[10];
   int brk = 0;

   ENTRY("Segment_ParseInput");
   
   Opt = (SEG_OPTS *)malloc(sizeof(SEG_OPTS));
   
   kar = 1;
   Opt->aset_name = NULL;
   Opt->mset_name = NULL;
   Opt->csfinit_name = NULL;
   Opt->wminit_name = NULL;
   Opt->gminit_name = NULL;
   Opt->prefix = NULL;
   Opt->aset = NULL;
   Opt->mset = NULL;
   Opt->csfinit = NULL;
   Opt->wminit = NULL;
   Opt->gminit = NULL;
   Opt->oset = NULL;
   Opt->debug = 0;
   Opt->idbg = Opt->kdbg = Opt->jdbg = -1;
   Opt->na = 4.0;
   Opt->DistMetric = SEG_METH_T;
   Opt->N_loopmax = 10;
   brk = 0;
	while (kar < argc) { /* loop accross command ine options */
		/*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
		if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
			 Segment_usage();
          exit (0);
		}
      
      #ifdef USE_TRACING
            if( strncmp(argv[kar],"-trace",5) == 0 ){
               DBG_trace = 1 ;
               brk = 1 ;
            }
            if( strncmp(argv[kar],"-TRACE",5) == 0 ){  
               DBG_trace = 2 ;
               brk = 1 ;
            }
      #endif
      
      if (!brk && (strcmp(argv[kar], "-debug") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need argument after -debug \n");
				exit (1);
			}
			Opt->debug = atoi(argv[kar]);
         brk = 1;
		}      

      if (!brk && (strcmp(argv[kar], "-max_loops") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need argument after -max_loops \n");
				exit (1);
			}
			Opt->N_loopmax = atoi(argv[kar]);
         brk = 1;
		}      
      
      if (!brk && (strcmp(argv[kar], "-sphere_hood") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need argument after -sphere_hood \n");
				exit (1);
			}
			Opt->na = atof(argv[kar]);
         brk = 1;
		} 
      
      if( strcmp(argv[kar],"-T_met") == 0 ){
         Opt->DistMetric = SEG_METH_T ;
         brk = 1;
      }
      
      if( strcmp(argv[kar],"-Dist_met") == 0 ){
         Opt->DistMetric = SEG_METH_DIST ;
         brk = 1;
      }
      
      if (!brk && (strcmp(argv[kar], "-vox_debug") == 0)) {
         kar ++;
			if (kar+2 >= argc)  {
		  		fprintf (stderr, "need 3 arguments after -vox_debug \n");
				exit (1);
			}
			Opt->idbg = atoi(argv[kar]); ++kar;
         Opt->jdbg = atoi(argv[kar]); ++kar;
         Opt->kdbg = atoi(argv[kar]);
         brk = 1;
		} 
     
      if (!brk && (strcmp(argv[kar], "-anat") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need argument after -anat \n");
				exit (1);
			}
			Opt->aset_name = argv[kar];
         brk = 1;
		}
            
      if (!brk && (strcmp(argv[kar], "-csf") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need argument after -csf \n");
				exit (1);
			}
			Opt->csfinit_name = argv[kar];
         brk = 1;
		}
      
      if (!brk && (strcmp(argv[kar], "-wm") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need argument after -wm \n");
				exit (1);
			}
			Opt->wminit_name = argv[kar];
         brk = 1;
		}

      if (!brk && (strcmp(argv[kar], "-mask") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need argument after -mask \n");
				exit (1);
			}
			Opt->mset_name = argv[kar];
         brk = 1;
		}
      
      if (!brk && (strcmp(argv[kar], "-gm") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need argument after -gm \n");
				exit (1);
			}
			Opt->gminit_name = argv[kar];
         brk = 1;
		}
      
      if (!brk && (strcmp(argv[kar], "-prefix") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need argument after -prefix \n");
				exit (1);
			}
			Opt->prefix = (argv[kar]);
         brk = 1;
		}
      
      if (!brk) {
			fprintf (stderr,"Option %s not understood. Try -help for usage\n", argv[kar]);
			exit (1);
		} else {	
			brk = 0;
			kar ++;
		}

   }
   if (!Opt->gminit_name || !Opt->csfinit_name || !Opt->wminit_name || !Opt->aset_name) {
      ERROR_exit("Missing input");
   }
   
   if (!Opt->prefix) Opt->prefix = "SegOut";
   
   RETURN(Opt);
}


int Segment(SEG_OPTS *Opt) 
{
   MCW_cluster *nbhd=NULL ;
   float dx , dy , dz ;
   int nxyz, nx, ny, nz, vstep, ijk, ii, jj, kk;
   byte *vval=NULL, *csfmask=NULL, *gmmask=NULL, *wmmask=NULL, *mmask=NULL;
   float *aval = NULL;
   MRI_IMAGE *nbim_csf=NULL, *nbim_wm=NULL, *nbim_gm=NULL;
   float dist_csf, dist_wm, dist_gm, min_dist, med_csf, med_wm, med_gm, mad_csf, mad_wm, mad_gm;
   float dist_mad_rat, dist_mad_rat_csf, dist_mad_rat_wm, dist_mad_rat_gm ;
   float min_T, T_csf, T_gm, T_wm, vval_candidate;
   byte bb=0;
   int ShowVoxSketchy=0, many = 0, far = 0, n_csf, n_gm, n_wm, n_m, iloop;
   
   ENTRY("Segment");
   
   /* prep output */
   Opt->oset  = EDIT_empty_copy( Opt->aset ) ;
   EDIT_dset_items( Opt->oset ,
                      ADN_nvals     , 5       ,
                      ADN_datum_all , MRI_byte   ,
                      ADN_brick_fac , NULL        ,
                      ADN_prefix    , Opt->prefix ,
                    ADN_none ) ;
   nx = DSET_NX(Opt->aset);
   ny = DSET_NY(Opt->aset);
   nz = DSET_NZ(Opt->aset);
   nxyz = DSET_NVOX(Opt->aset);
   
   csfmask = THD_makemask( Opt->csfinit, 0 , 1.0 , 0.0 );
   gmmask  = THD_makemask( Opt->gminit , 0 , 1.0 , 0.0 );
   wmmask  = THD_makemask( Opt->wminit , 0 , 1.0 , 0.0 );
   if (Opt->mset) mmask = THD_makemask( Opt->mset , 0 , 1.0 , 0.0 );
   else mmask = THD_makemask( Opt->aset , 0 , 1.0 , 0.0 );
   aval = (float *)malloc(sizeof(float)*nxyz);
   vval = (byte *)calloc(nxyz, sizeof(byte));
   if (!vval || !csfmask || !gmmask || !wmmask || !aval || !mmask) {
      ERROR_exit("Failed to allocate for vval or masks");
   }
   
   EDIT_coerce_scale_type( DSET_NVOX(Opt->aset) , DSET_BRICK_FACTOR(Opt->aset,0) ,
                  DSET_BRICK_TYPE(Opt->aset,0), DSET_ARRAY(Opt->aset, 0) ,      /* input  */
                  MRI_float               , aval  ) ;   /* output */

   if( Opt->na < 0.0f ){ dx = dy = dz = 1.0f ; Opt->na = -Opt->na ; }
   else         { dx = fabsf(DSET_DX(Opt->aset)) ;
                  dy = fabsf(DSET_DY(Opt->aset)) ;
                  dz = fabsf(DSET_DZ(Opt->aset)) ; }
                  
   nbhd = MCW_spheremask( dx,dy,dz , Opt->na ) ;
   if (Opt->debug) {
      fprintf(stderr,"nbhd: %p\n"
                     "%d voxels.\n",
                     nbhd, nbhd->num_pt);
   }
   
   
   iloop = 0;
   do {
      n_csf = THD_countmask(nxyz, csfmask);
      n_gm  = THD_countmask(nxyz, gmmask );
      n_wm  = THD_countmask(nxyz, wmmask );
      n_m   = THD_countmask(nxyz,  mmask );

      if (Opt->debug) {
         fprintf(stderr,"Pass %d, have:\n"
                        "%d voxels in csf,\n"
                        "%d voxels in gm,\n"
                        "%d voxels in wm\n"
                        "%.2f%% gm/wm\n"
                        "%d voxels in mask\n",
                        iloop, n_csf,
                        n_gm,
                        n_wm,
                        (float)n_gm/(float)n_wm*100.0f,
                        n_m);
      }

      vstep = (Opt->debug && nxyz > 99999) ? nxyz/50 : 0 ;
      if( vstep ) fprintf(stderr,"++ voxel loop:") ;

      SetSearchAboutMaskedVoxel(1); /* search the nighborhood of a voxel that is itself not in the mask 
                                       The default behaviour for  THD_get_dset_nbhd is to return a NULL
                                       if the voxel ii, jj, kk itself is not in the mask   */
      /* for each voxel in aset */
        far = 0; many = 0;
        for( ijk=kk=0 ; kk < nz ; kk++ ){
         for( jj=0 ; jj < ny ; jj++ ){
          for( ii=0 ; ii < nx ; ii++,ijk++ ){
            if( vstep && ijk%vstep==vstep-1 ) vstep_print() ;
            if (mmask[ijk] && ( Opt->debug < 3 || (Opt->debug > 2 && Opt->idbg == ii && Opt->jdbg == jj && Opt->kdbg == kk) ) ) {
               nbim_csf = THD_get_dset_nbhd( Opt->aset , 0  , csfmask , ii,jj,kk , nbhd ) ; 
               nbim_wm  = THD_get_dset_nbhd( Opt->aset , 0  , wmmask  , ii,jj,kk , nbhd ) ;
               nbim_gm  = THD_get_dset_nbhd( Opt->aset , 0  , gmmask  , ii,jj,kk , nbhd ) ;

               dist_csf = dist_wm = dist_gm = -1.0;
               if (nbim_csf) {
                  med_csf = mri_nstat( NSTAT_MEDIAN , nbim_csf ) ;
                  mad_csf = mri_nstat( NSTAT_MAD    , nbim_csf ) ;
                  dist_csf = med_csf - aval[ijk]; if (dist_csf < 0.0) dist_csf = -dist_csf;
               } else {
                  if (Opt->debug > 2) fprintf(stderr,"NULL nbim_csf @[%d %d %d]\n", ii, jj, kk);
                  med_csf = -1.0;
                  mad_csf = -1.0;
               }
               if (nbim_wm) {
                  med_wm = mri_nstat( NSTAT_MEDIAN , nbim_wm ) ;
                  mad_wm = mri_nstat( NSTAT_MAD    , nbim_wm ) ;
                  dist_wm  = med_wm  - aval[ijk]; if (dist_wm  < 0.0) dist_wm  = -dist_wm;
               } else {
                  if (Opt->debug > 2) fprintf(stderr,"NULL nbim_wm @[%d %d %d]\n", ii, jj, kk);
                  med_wm = -1.0;
                  mad_wm = -1.0;
               }
               if (nbim_gm) {
                  med_gm  = mri_nstat( NSTAT_MEDIAN , nbim_gm ) ;
                  mad_gm = mri_nstat( NSTAT_MAD    , nbim_gm ) ;
                  dist_gm  = med_gm  - aval[ijk]; if (dist_gm  < 0.0) dist_gm  = -dist_gm;
               } else {
                  if (Opt->debug > 2) fprintf(stderr,"NULL nbim_gm @[%d %d %d]\n", ii, jj, kk);
                  med_gm = -1.0;
                  mad_wm = -1.0;
               }


               if (Opt->DistMetric == SEG_METH_DIST) {    /* distance based, what is the closest to Opt->aset[ijk] ? */
                  {
                     vval_candidate = SEG_UNKNOWN;   min_dist = 1e30;  
                     dist_mad_rat = dist_mad_rat_csf = dist_mad_rat_wm = dist_mad_rat_gm = 1e30;
                     if (dist_csf >= 0.0) {
                        dist_mad_rat_csf = dist_csf / mad_csf;
                        if (dist_csf < min_dist) { vval_candidate = SEG_CSF; min_dist = dist_csf; dist_mad_rat = dist_mad_rat_csf; }
                     }
                     if (dist_gm  >= 0.0) { 
                        dist_mad_rat_gm = dist_gm / mad_gm;
                        if (dist_gm  < min_dist) { vval_candidate = SEG_GM ; min_dist = dist_gm;  dist_mad_rat = dist_mad_rat_gm;  }  
                     }
                     if (dist_wm  >= 0.0) { 
                        dist_mad_rat_wm = dist_wm / mad_wm;
                        if (dist_wm  < min_dist) { vval_candidate = SEG_WM ; min_dist = dist_wm;  dist_mad_rat = dist_mad_rat_wm;  }
                     }
                  }
                  /* some checks to see if decision was somewhat shaky */
                  ShowVoxSketchy = 0;
                  if (dist_mad_rat > 2.0) { /* This decision is shaky, value quite far from median */ 
                     ShowVoxSketchy = 1;
                     ++far;   
                  } else { /* decision is OK, do we have other possible ones? */
                     if (vval_candidate == SEG_CSF) {
                        if (dist_mad_rat_wm < 2.0 || dist_mad_rat_gm < 2.0) {
                           ShowVoxSketchy = 2;
                           ++many;
                        }
                     }else if (vval_candidate == SEG_GM) {
                        if (dist_mad_rat_csf < 2.0 || dist_mad_rat_wm < 2.0) {
                           ShowVoxSketchy = 2;
                           ++many;
                        }
                     }else if (vval_candidate == SEG_WM) {
                        if (dist_mad_rat_csf < 2.0 || dist_mad_rat_gm < 2.0) {
                           ShowVoxSketchy = 2;
                           ++many;
                        }
                     }
                  }
                  
                  if (!ShowVoxSketchy) {
                     /* congrats, get that voxel out of the mask */
                     vval[ijk] = vval_candidate;
                     mmask[ijk] = 0;
                  } else {
                     if (iloop == Opt->N_loopmax) {
                        /* take what you can get */
                        vval[ijk] = vval_candidate;
                        /* don't set mmask[ijk] = 0;, leave mmask's contents to flag sketchy voxels 
                           mmask will reflect the choice made for sketchy voxels*/
                        mmask[ijk] = vval_candidate;
                     }
                  }
                  
                  if ((Opt->debug > 1 && ShowVoxSketchy) || (Opt->idbg == ii && Opt->jdbg == jj && Opt->kdbg == kk)) {
                     if (ShowVoxSketchy == 1) fprintf(stdout,"\nSketchy, nothing close ");
                     else if (ShowVoxSketchy == 2) fprintf(stdout,"\nSketchy, many close ");
                     else fprintf(stdout,"\nDebug ");
                     fprintf(stdout,"for voxel [%d %d %d], pass %d\n"
                                    "aval    = %.2f\n"
                                    "med:mad_csf = %.2f:%.2f, dist_csf = %.2f, dist_mad_rat_csf = %.2f, (%d) voxels in hood\n"
                                    "med:mad_gm  = %.2f:%.2f, dist_gm  = %.2f, dist_mad_rat_gm  = %.2f, (%d) voxels in hood\n"
                                    "med:mad_wm  = %.2f:%.2f, dist_wm  = %.2f, dist_mad_rat_wm  = %.2f, (%d) voxels in hood\n"
                                    "Voxel set to %s\n",
                                    ii, jj, kk, iloop, 
                                    aval[ijk],
                                    med_csf, mad_csf, dist_csf,  (dist_mad_rat_csf < 5000 ? dist_mad_rat_csf : -1.0), ((nbim_csf) ? nbim_csf->nvox : 0),
                                    med_gm , mad_gm,  dist_gm,   (dist_mad_rat_gm  < 5000 ? dist_mad_rat_gm  : -1.0), ((nbim_gm)  ? nbim_gm->nvox  : 0),
                                    med_wm , mad_wm,  dist_wm,   (dist_mad_rat_wm  < 5000 ? dist_mad_rat_wm  : -1.0), ((nbim_wm)  ? nbim_wm->nvox  : 0),
                                    SegValToSegName(vval[ijk]));
                  }
               } else if (Opt->DistMetric == SEG_METH_T) {    /* T based, what is Opt->aset[ijk] closest to, based on the T value ?  (Forgive me Stat Gods)*/
                  vval[ijk] = SEG_UNKNOWN;   min_dist = 1e30;  min_T = 1e30;
                  T_csf = T_wm = T_gm = 1e30;
                  if (dist_csf >= 0.0) {
                     T_csf = dist_csf / ( mad_csf / sqrt (nbim_csf->nvox) );
                     if (T_csf < min_T) { vval[ijk] = SEG_CSF; min_T = T_csf; }
                  }
                  if (dist_gm  >= 0.0) { 
                     T_gm = dist_gm / ( mad_gm / sqrt (nbim_gm->nvox) );
                     if (T_gm  < min_T) { vval[ijk] = SEG_GM ; min_T = T_gm;  }  
                  }
                  if (dist_wm  >= 0.0) { 
                     T_wm = dist_wm / ( mad_wm / sqrt (nbim_wm->nvox) );
                     if (T_wm  < min_T) { vval[ijk] = SEG_WM ; min_T = T_wm;  }
                  }
                  if ((Opt->idbg == ii && Opt->jdbg == jj && Opt->kdbg == kk)) {
                     fprintf(stdout,"\nDebug ");
                     fprintf(stdout,"for voxel [%d %d %d]\n"
                                    "aval    = %.2f\n"
                                    "med:mad_csf = %.2f:%.2f, dist_csf = %.2f, T_csf = %.2f, (%d) voxels in hood\n"
                                    "med:mad_gm  = %.2f:%.2f, dist_gm  = %.2f, T_gm  = %.2f, (%d) voxels in hood\n"
                                    "med:mad_wm  = %.2f:%.2f, dist_wm  = %.2f, T_wm  = %.2f, (%d) voxels in hood\n"
                                    "Voxel set to %s\n",
                                    ii, jj, kk,
                                    aval[ijk],
                                    med_csf, mad_csf, dist_csf,  (T_csf < 5000 ? T_csf : -1.0), ((nbim_csf) ? nbim_csf->nvox : 0),
                                    med_gm , mad_gm,  dist_gm,   (T_gm  < 5000 ? T_gm  : -1.0), ((nbim_gm)  ? nbim_gm->nvox  : 0),
                                    med_wm , mad_wm,  dist_wm,   (T_wm  < 5000 ? T_wm  : -1.0), ((nbim_wm)  ? nbim_wm->nvox  : 0),
                                    SegValToSegName(vval[ijk]));
                  }
               }
               mri_free(nbim_csf) ;
               mri_free(nbim_wm) ;
               mri_free(nbim_gm) ;
            }
        }}}

      if (Opt->debug) {
         if (Opt->DistMetric == SEG_METH_DIST) {
            fprintf(stderr,"\nPass %d:\n"
                           "Out of %d voxels in mask\n"
                           "       %d (%.2f%%) were within 2 MAD of only one class\n"
                           "       %d (%.2f%%) were far\n"
                           "       %d (%.2f%%) had many close options\n",
                           iloop, n_m, 
                           n_m - (far + many), (float)(n_m - (far + many))/(float)n_m*100.0,
                           far               , (float)far                 /(float)n_m*100.0,
                           many              , (float)many                /(float)n_m*100.0 ); 
         } else if (Opt->DistMetric == SEG_METH_T) {

         }
      }   
      if (iloop == 0) { /* reset masks from initial guess to voxels that were a sure thing */
         for (ijk=0; ijk<nxyz; ++ijk) {
            csfmask[ijk] = wmmask[ijk] = gmmask[ijk] = SEG_UNKNOWN;
         }
      }
      /* Update masks to reflect current result */
      for (ijk=0; ijk<nxyz; ++ijk) {
         if (vval[ijk] == SEG_CSF) csfmask[ijk] = SEG_CSF;
         else if (vval[ijk] == SEG_GM) gmmask[ijk] = SEG_GM;
         else if (vval[ijk] == SEG_WM) wmmask[ijk] = SEG_WM;
      }
      
      
      ++iloop;
   } while (iloop <= Opt->N_loopmax && Opt->DistMetric == SEG_METH_DIST);
   
   /* report final count */
   if (Opt->debug) {
      n_csf = THD_countmask(nxyz, csfmask);
      n_gm  = THD_countmask(nxyz, gmmask );
      n_wm  = THD_countmask(nxyz, wmmask );
      n_m   = THD_countmask(nxyz,  mmask );

      fprintf(stderr,"Final result:\n"
                     "%d voxels in csf,\n"
                     "%d voxels in gm,\n"
                     "%d voxels in wm\n"
                     "%.2f%% gm/wm\n"
                     "%d voxels were still sketchy but decided upon anyway\n",
                     n_csf,
                     n_gm,
                     n_wm,
                     (float)n_gm/(float)n_wm*100.0f,
                     n_m);
   }

   EDIT_substitute_brick(Opt->oset, 0, MRI_byte, vval );
   EDIT_dset_items (Opt->oset, ADN_brick_label_one + 0, "All_Masks", ADN_none);
   EDIT_substitute_brick(Opt->oset, 1, MRI_byte, wmmask );
   EDIT_dset_items (Opt->oset, ADN_brick_label_one + 1, "White", ADN_none);
   EDIT_substitute_brick(Opt->oset, 2, MRI_byte, gmmask );
   EDIT_dset_items (Opt->oset, ADN_brick_label_one + 2, "Gray", ADN_none);
   EDIT_substitute_brick(Opt->oset, 3, MRI_byte, csfmask );
   EDIT_dset_items (Opt->oset, ADN_brick_label_one + 3, "CSF", ADN_none);
   EDIT_substitute_brick(Opt->oset, 4, MRI_byte, mmask );
   EDIT_dset_items (Opt->oset, ADN_brick_label_one + 4, "Sketcht", ADN_none);
   
   KILL_CLUSTER(nbhd); nbhd = NULL;
   free(aval); aval = NULL;
   if (0) { /* free no more, now being placed in oset ... */
      free(csfmask); free(gmmask); free(wmmask); 
      csfmask = NULL; gmmask = NULL; wmmask = NULL; 
      free(mmask); mmask = NULL;
   }
          
   RETURN(1);
}
   
int main(int argc, char **argv)
{
   SEG_OPTS *Opt=NULL;
   
   mainENTRY("Segment");
   
   Opt = Segment_ParseInput (argv,  argc);
   
   /* load the input data */
   Opt->aset = THD_open_dataset( Opt->aset_name );
   if( !ISVALID_DSET(Opt->aset) ){
     fprintf(stderr,"**ERROR: can't open dataset %s\n",Opt->aset_name) ;
     exit(1);
   }
   /*
   Opt->mset = THD_open_dataset( Opt->mset_name );
   if( !ISVALID_DSET(Opt->mset) ){
     fprintf(stderr,"**ERROR: can't open dataset %s\n",Opt->mset_name) ;
     exit(1);
   }
   */
   Opt->wminit = THD_open_dataset( Opt->wminit_name );
   if( !ISVALID_DSET(Opt->wminit) ){
     fprintf(stderr,"**ERROR: can't open dataset %s\n",Opt->wminit_name) ;
     exit(1);
   }
   Opt->gminit = THD_open_dataset( Opt->gminit_name );
   if( !ISVALID_DSET(Opt->gminit) ){
     fprintf(stderr,"**ERROR: can't open dataset %s\n",Opt->gminit_name) ;
     exit(1);
   }
   Opt->csfinit = THD_open_dataset( Opt->csfinit_name );
   if( !ISVALID_DSET(Opt->gminit) ){
     fprintf(stderr,"**ERROR: can't open dataset %s\n",Opt->csfinit_name) ;
     exit(1);
   }
   
   DSET_mallocize(Opt->aset)   ; DSET_load(Opt->aset);
   DSET_mallocize(Opt->gminit) ; DSET_load(Opt->gminit);
   DSET_mallocize(Opt->wminit) ; DSET_load(Opt->wminit); 
   DSET_mallocize(Opt->csfinit); DSET_load(Opt->csfinit); 

   /* check on sizes */
   if (     DSET_NX(Opt->aset) != DSET_NX(Opt->gminit) 
         || DSET_NX(Opt->aset) != DSET_NX(Opt->wminit) 
         || DSET_NX(Opt->aset) != DSET_NX(Opt->gminit)
         || DSET_NY(Opt->aset) != DSET_NY(Opt->gminit) 
         || DSET_NY(Opt->aset) != DSET_NY(Opt->wminit) 
         || DSET_NY(Opt->aset) != DSET_NY(Opt->gminit)
         || DSET_NZ(Opt->aset) != DSET_NZ(Opt->gminit) 
         || DSET_NZ(Opt->aset) != DSET_NZ(Opt->wminit) 
         || DSET_NZ(Opt->aset) != DSET_NZ(Opt->gminit)   ) {
      ERROR_exit("All input data must have same grid");        
   }
   
   if (Opt->mset) { DSET_mallocize(Opt->mset); DSET_load(Opt->mset); }
   if (!Segment(Opt)) {
      ERROR_exit("Failed in Segment");
   }
   
   /* write output */
   DSET_write(Opt->oset);
   
   /* all done, free */
   DSET_delete(Opt->aset); Opt->aset = NULL;
   DSET_delete(Opt->mset); Opt->mset = NULL;
   DSET_delete(Opt->oset); Opt->oset = NULL;
   DSET_delete(Opt->gminit); Opt->gminit = NULL;
   DSET_delete(Opt->wminit); Opt->wminit = NULL;
   DSET_delete(Opt->csfinit); Opt->csfinit = NULL;
   free(Opt); Opt = NULL;
   
   exit(0);
}
