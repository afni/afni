#include <stdio.h>
#include <stdlib.h> 
#include <string.h>

#include "../mrilib.h"
#include "cluster_floatNOMASK.h"
#include "thd_segtools_fNM.h"


// from command.c file

static void display_help(void)
{ printf ("Clustering 4segmentation, command-line version.\n");
  printf ("    Based on The C clustering library.\n");
  printf ("    Copyright (C) 2002 Michiel Jan Laurens de Hoon.\n");
  printf ("USAGE: cluster [options]\n");
  printf ("options:\n");
  printf ("  -v, --version Version information\n");
  printf ("  -f filename   File loading\n");
  printf ("                You can specify multiple filenames in sequence\n"
          "                and they will be catenated internally.\n"
          "         e.g: -f F1+orig F2+orig F3+orig ...\n"
          "           or -f F1+orig -f F2+orig -f F3+orig ...\n" );
  printf(
 " -mask mset   Means to use the dataset 'mset' as a mask:\n"
 "                 Only voxels with nonzero values in 'mset'\n"
 "                 will be printed from 'dataset'.  Note\n"
 "                 that the mask dataset and the input dataset\n"
 "                 must have the same number of voxels.\n"
 " -mrange a b  Means to further restrict the voxels from\n"
 "                 'mset' so that only those mask values\n"
 "                 between 'a' and 'b' (inclusive) will\n"
 "                 be used.  If this option is not given,\n"
 "                 all nonzero values from 'mset' are used.\n"
 "                 Note that if a voxel is zero in 'mset', then\n"
 "                 it won't be included, even if a < 0 < b.\n"
 " -cmask 'opts' Means to execute the options enclosed in single\n"
 "                  quotes as a 3dcalc-like program, and produce\n"
 "                  produce a mask from the resulting 3D brick.\n"
 "       Examples:\n"
 "        -cmask '-a fred+orig[7] -b zork+orig[3] -expr step(a-b)'\n"
 "                  produces a mask that is nonzero only where\n"
 "                  the 7th sub-brick of fred+orig is larger than\n"
 "                  the 3rd sub-brick of zork+orig.\n"
 "        -cmask '-a fred+orig -expr 1-bool(k-7)'\n"
 "                  produces a mask that is nonzero only in the\n"
 "                  7th slice (k=7); combined with -mask, you\n"
 "                  could use this to extract just selected voxels\n"
 "                  from particular slice(s).\n"
 "       Notes: * You can use both -mask and -cmask in the same\n"
 "                  run - in this case, only voxels present in\n"
 "                  both masks will be dumped.\n"
 "              * Only single sub-brick calculations can be\n"
 "                  used in the 3dcalc-like calculations -\n"
 "                  if you input a multi-brick dataset here,\n"
 "                  without using a sub-brick index, then only\n"
 "                  its 0th sub-brick will be used.\n"
 "              * Do not use quotes inside the 'opts' string!\n"
 "\n");
  printf ("  -cg a|m       Specifies whether to center each row\n"
          "                in the data\n"
          "                a: Subtract the mean of each row\n"
          "                m: Subtract the median of each row\n"
          "                (default is no centering)\n");
  printf ("  -ng           Specifies to normalize each row in the data\n"
          "                (default is no normalization)\n");
  printf ("  -ca a|m       Specifies whether to center each column \n"
          "                in the data\n"
          "                a: Subtract the mean of each column\n"
          "                m: Subtract the median of each column\n"
          "                (default is no centering)\n");
  printf ("  -na           Specifies to normalize each column in the data\n"
          "                (default is no normalization)\n");
  printf ("  -u jobname    Allows you to specify a different name for the \n"
          "                output files.\n"
          "                (default is derived from the input file name)\n");
  printf ("  -prefix PREFIX Allows you to specify a prefix for the output \n"
          "                 volumes. Default is the same as jobname\n");
  printf ("  -g [0..8]     Specifies distance measure for gene clustering\n" );
  printf ("                Note: Weight is a vector as long as the signatures\n"
          "                and used when computing distances. However for the\n"
          "                moment, all weights are set to 1\n"
          "                0: No gene clustering\n"
          "                1: Uncentered correlation distance\n"
          "                    Same as Pearson distance, except\n"
          "                    the means of v and s are not removed\n"
          "                    when computing correlation.\n"
          "                2: Pearson distance\n"
          "                    = (1-Weighted_Pearson_Correlation(v,s))\n"
          "                3: Uncentered correlation distance, absolute value\n"
          "                    Same as abs(Pearson distance), except\n"
          "                    the means of v and s are not removed\n"
          "                    when computing correlation.\n"
          "                4: Pearson distance, absolute value\n"
          "                    = (1-abs(Weighted_Pearson_Correlation(v,s)))\n"
          "                5: Spearman's rank distance\n"
          "                    = (1-Spearman_Rank_Correlation(v,s))\n"
          "                   No weighting is used\n"
          "                6: Kendall's distance\n"
          "                    = (1-Kendall_Tau(v,s))\n"
          "                   No weighting is used\n"
          "                7: Euclidean distance between v and s\n"
          "                    = 1/sum(weight) * sum(weight[i]*(v[i]-s[i])^2)\n"
          "                8: City-block distance\n"
          "                    = 1/sum(weight) * sum(weight[i]*abs(v[i]-s[i]))\n"
          "\n"
          "                (default for -g is 1)\n");
  printf ("  -k number     Specifies whether to run k-means clustering\n"
          "                instead of hierarchical clustering, and the number\n"
          "                of clusters k to use. \n"
          "                Default is kmeans with k = 3 clusters\n");
  printf ("  -c number     Force the program to do hierarchical clsutering\n"
          "                and specifies the number of clusters for tree\n"
          "                cutting after hierarchical clustering.\n"
          "       Options -c and -k are mutually exclusive\n");
  printf ("  -r number     For k-means clustering, the number of times the\n"
          "                k-means clustering algorithm is run\n"
          "                (default: 1)\n");
  printf ("  -m [msca]     Specifies which hierarchical clustering method to\n"
          "                use:\n"
          "                m: Pairwise complete-linkage\n"
          "                s: Pairwise single-linkage\n"
          "                c: Pairwise centroid-linkage\n"
          "                a: Pairwise average-linkage\n"
          "                (default: m)\n");
  printf ("  -rsigs SIGS   Calculate distances from each voxel's signature\n"
          "                to the signatures in SIGS. \n"
          "                SIGS is a multi-column 1D file with each column\n"
          "                being a signature.\n"
          "                The output is a dset the same size as the input\n"
          "                with as many sub-bricks as there are columns in \n"
          "                SIGS.\n"
          "                With this option, no clustering is done.\n");
  printf ("  -verb         verbose \n");
  printf ("  -voxdbg I J K Output debugging info for voxel I J K\n");    
  printf ("  -seed SEED    Seed for the random number generator.\n"
          "                Default is 1234567\n");    
  EXRETURN;
}



/* ========================================================================= */

int main(int argc, char **argv)
{ 
   int ii=0, ncol=0, nrow=0, nl=0, nc=0, posi=0, posj=0, posk=0;
   //int nclust=atoi(argv[2]);

   //from command.c

   int i = 1;
   char* filename[256];
   int l = 0;
   int s = 0;
   int x = 2;
   int y = 1;
   int Rows, Columns;
   char arraymetric = '\0';
   char method = 'm';
   char cg = '\0';
   char ca = '\0';
   int ng = 0;
   int na = 0;
   char *prefix = NULL;
   char *signame=NULL;
   THD_3dim_dataset *in_set=NULL, *clust_set=NULL;
   THD_3dim_dataset *mask_dset=NULL, *dist_set=NULL;
   byte *cmask=NULL ; int ncmask=0 ;
   byte *mask=NULL;
   int nmask=-1, mnx=-1, mny=-1, mnz=-1, iset=0, N_iset=0, mnxyz=-1;
   float mask_bot=666.0 , mask_top=-666.0 ;
   OPT_KMEANS oc;
   float *dvec=NULL, **D=NULL;
   int n = 0, Ncoltot=0, nc0=0, nx=0, ny=0, nz=0;
   char *prefixvcd = NULL;

   mainENTRY("3dAclustering_fNM"); machdep();
   PRINT_VERSION("3dAclustering_fNM"); AUTHOR("avovk") ;
   
   oc.r = 1;
   oc.k = 0;
   oc.kh = 0;
   oc.jobname = NULL;
   oc.distmetric = 'u';
   oc.verb = 0;
   oc.rand_seed = 1234567;
   for (i=0; i<4; ++i) oc.voxdebug[i] = -1;
   N_iset = 0;
   filename[N_iset] = NULL;
   
   if (argc < 2) {
      display_help();
      RETURN(0);
   }

   i = 1;
   while (i < argc)
   { const char* const argument = argv[i];
    i++;
    if (strlen(argument)<2)
    { printf("ERROR: missing argument\n");
      RETURN(1);
    }
    if (argument[0]!='-')
    { printf("ERROR: unknown argument %s\n", argument);
      RETURN(1);
    }
    if(!strcmp(argument,"--version") || !strcmp(argument,"-v"))
    { clusterlib_display_version();
      RETURN(0);
    }
    if(     !strcmp(argument,"--help") 
         || !strcmp(argument,"-h") 
         || !strcmp(argument,"-help") )
    { display_help();
      RETURN(0);
    }
    if(     !strcmp(argument,"--verb") 
         || !strcmp(argument,"-verb") )
    { oc.verb=1;
      continue;
    }
    if(!strcmp(argument,"-cg"))
    { if (i==argc || strlen(argv[i])>1 || !strchr("am",argv[i][0]))
      { printf ("Error reading command line argument cg\n");
        RETURN(1);
      }
      cg = argv[i][0];
      i++;
      continue;
    }
    if(!strcmp(argument,"-ca"))
    { if (i==argc || strlen(argv[i])>1 || !strchr("am",argv[i][0]))
      { printf ("Error reading command line argument ca\n");
        RETURN(1);
      }
      ca = argv[i][0];
      i++;
      continue;
    }
    if(!strcmp(argument,"-prefix"))
    { if (i==argc)
      { printf ("Error: Need name after -prefix\n");
        RETURN(1);
      }
      prefix = argv[i];
      i++;
      continue;
    }
    if(!strcmp(argument,"-voxdbg"))
    { if (i+2==argc)
      { printf ("Error: Need 3 integers after -voxedbg\n");
        RETURN(1);
      }
      oc.voxdebug[0] = atoi(argv[i]);i++;
      oc.voxdebug[1] = atoi(argv[i]);i++;
      oc.voxdebug[2] = atoi(argv[i]);i++;
      continue;
    }

    if(!strcmp(argument,"-rsigs"))
    { if (i==argc)
      { printf ("Error: Need name after -rsigs\n");
        RETURN(1);
      }
      signame = argv[i];
      i++;
      continue;
    }
    
    if(!strcmp(argument,"-seed"))
    { if (i==argc)
      { printf ("Error: Need a +ve integer after -seed\n");
        RETURN(1);
      }
      oc.rand_seed = atoi(argv[i]);
      if (oc.rand_seed <=0) {
        printf ("Error: seed must be > 0\n");
        RETURN(1);
      }
      i++;
      continue;
    }
    
    if( strncmp(argument,"-mask",5) == 0 ){
       if( mask_dset != NULL )
         ERROR_exit("Cannot have two -mask options!\n") ;
       if( i >= argc )
         ERROR_exit("-mask option requires a following argument!\n");
       mask_dset = THD_open_dataset( argv[i] ) ;
       if( mask_dset == NULL )
         ERROR_exit("Cannot open mask dataset!\n") ;
       if( DSET_BRICK_TYPE(mask_dset,0) == MRI_complex )
         ERROR_exit("Cannot deal with complex-valued mask dataset!\n");
       i++ ; continue ;
    }

    if( strncmp(argument,"-mrange",5) == 0 ){
      if( i+1 >= argc )
        ERROR_exit("-mrange option requires 2 following arguments!\n");
       mask_bot = strtod( argv[  i] , NULL ) ;
       mask_top = strtod( argv[++i] , NULL ) ;
       if( mask_top < mask_top )
         ERROR_exit("-mrange inputs are illegal!\n") ;
       i++ ; continue ;
    }

    if( strcmp(argument,"-cmask") == 0 ){  /* 16 Mar 2000 */
       if( i >= argc )
          ERROR_exit("-cmask option requires a following argument!\n");
       cmask = EDT_calcmask( argv[i] , &ncmask, 0 ) ;
       if( cmask == NULL ) ERROR_exit("Can't compute -cmask!\n");
       i++ ; continue ;
    }
      
    if(!strcmp(argument,"-ng"))
    { ng = 1;
      continue;
    }
    if(!strcmp(argument,"-na"))
    { na = 1;
      continue;
    }
    switch (argument[1])
    { case 'l': l=1; break;
      case 'u':
      { if (i==argc)
        { printf ("Error reading command line argument u: "
                  "no job name specified\n");
          RETURN(1);
        }
        oc.jobname = clusterlib_setjobname(argv[i],0);
        i++;
        break;
      }
      case 'f':
      { if (i==argc)
        { printf ("Error reading command line argument f: "
                  "no file name specified\n");
          RETURN(1);
        }
        do {
         filename[N_iset] = argv[i];
         if (N_iset > 100) {
            printf ("Error: Too many input files!\n");
            RETURN(1);
         }
         ++N_iset; filename[N_iset] = NULL;
         i++;
        } while (i< argc && argv[i][0] != '-');
        break;
      }
      case 'g':
      { int g;
        if (i==argc)
        { printf ("Error reading command line argument g: parameter missing\n");
          RETURN(1);
        }
        g = clusterlib_readnumber(argv[i]);
        if (g < 0 || g > 9)
        { printf ("Error reading command line argument g: "
                  "should be between 0 and 9 inclusive\n");
          RETURN(1);
        }
        i++;
        oc.distmetric = clusterlib_getmetric(g);
        break;
      }

      case 'k':
      { if (i==argc)
        { printf ("Error reading command line argument k: "
                  "parameter missing\n");
          RETURN(1);
        }
        if (oc.kh > 0) {
            ERROR_message("-k and -c options are mutually exclusive\n");
            RETURN(1);
        }
        oc.k = clusterlib_readnumber(argv[i]);
        if (oc.k < 1)
        { printf ("Error reading command line argument k: "
                  "a positive integer is required\n");
          RETURN(1);
        }
        i++;
        break;
      }
    case 'c':
      { if (i==argc)
        { printf ("Error reading command line argument c: parameter missing\n");
          RETURN(1);
        }
        if (oc.k > 0) {
            ERROR_message("-k and -c options are mutually exclusive\n");
            RETURN(1);
        }
        oc.kh = clusterlib_readnumber(argv[i]);
        if (oc.kh < 1)
        { printf ("Error reading command line argument c: "
                  "a positive integer is required\n");
          RETURN(1);
        }
        i++;
        break;
      }
   case 'r':
      { if (i==argc)
        { printf ("Error reading command line argument r: parameter missing\n");
          RETURN(1);
        }
        oc.r = clusterlib_readnumber(argv[i]);
        if (oc.r < 1)
        { printf ("Error reading command line argument r: "
                  "a positive integer is required\n");
          RETURN(1);
        }
        i++;
        break;
      }
   case 'm':
      { if (i==argc || strlen(argv[i])>1 || !strchr("msca",argv[i][0]))
	  { printf ("Error reading command line argument m: "
               "should be 'm', 's', 'c', or 'a'\n");
	    RETURN(1);
	  }
        method = argv[i][0];
        i++;
        break;
      }
      default: 
         printf ("Unknown option %s\n", argv[i-1]);
         RETURN(1);
    }
    
   }
   if (oc.k <= 0 && oc.kh <= 0) oc.k = 3;
   
   if(oc.jobname == NULL) oc.jobname = clusterlib_setjobname(filename[0],1);

   
   /* load dsets and prepare array data for sending to clustering functions */
   
   if (!prefix) {
      prefix = oc.jobname; /* used to be "clusty" */
      THD_force_ok_overwrite(1) ;   /* don't worry about overwriting */
   }
   
   /* ------------- Mask business -----------------*/
   if( mask_dset == NULL ){
      mask = NULL ;
      if( oc.verb ) 
         INFO_message("Using all voxels in the entire dataset (no mask)\n") ;
   } else {
      mnx = DSET_NX(mask_dset); 
      mny = DSET_NY(mask_dset); 
      mnz = DSET_NZ(mask_dset);
      mnxyz = mnx*mny*mnz;
      mask = THD_makemask( mask_dset , 0 , mask_bot, mask_top ) ;
      if( mask == NULL ) ERROR_exit("Can't make mask") ;
      nmask = THD_countmask( mnx*mny*mnz  , mask ) ;
      if( oc.verb ) 
         INFO_message("%d voxels in the [%dx%dx%d] mask",nmask, mnx, mny, mnz) ;
      if( nmask <= 0 ) ERROR_exit("No voxels in the mask!\n") ;
      DSET_delete(mask_dset) ;
   }

   if( cmask != NULL ){
      if( mask != NULL ){
         if (mnxyz != ncmask) ERROR_exit("Mask and cmask dimension mismatch") ;
         for( ii=0 ; ii < mnxyz ; ii++ ) 
            mask[ii] = (mask[ii] && cmask[ii]) ;
         free(cmask) ;
         nmask = THD_countmask( mnxyz , mask ) ;
         if( nmask <= 0 ) ERROR_exit("No voxels in the mask+cmask!\n") ;
         if( oc.verb ) INFO_message("%d voxels in the mask+cmask\n",nmask) ;
      } else {
         mnx = -1; mny = 11; mnz = -1; /* unknown */
         mnxyz = ncmask;
         mask = cmask ;
         nmask = THD_countmask( mnxyz , mask ) ;
         if( nmask <= 0 ) ERROR_exit("No voxels in the cmask!\n") ;
         if( oc.verb ) INFO_message("%d voxels in the cmask\n",nmask) ;
      }
   }
   
   if (signame) {
      MRI_IMAGE *im = NULL;
      float *far = NULL;
      
      /* catenate all input dsets */
      if (N_iset == 1) {
         in_set = THD_open_dataset(filename[0]);
         CHECK_OPEN_ERROR(in_set,filename[0]) ;
         if (oc.voxdebug[0] >= 0) {
            /* setup for debugging */
            oc.voxdebug[3] = oc.voxdebug[0] + oc.voxdebug[1]*DSET_NX(in_set) +
                              oc.voxdebug[2]*DSET_NX(in_set)*DSET_NY(in_set);
         } else oc.voxdebug[3] = -1;
      } else {
         /* you'll need to read and catenate on the fly ... */
         ERROR_exit( "Not ready to deal with more than one input.\n"
                     "Consdier catenating the input externally.\n"
                     "Let me know if it becomes annoying ...\n");
      }
      
      /* load the set of distance files */
      im = mri_read_1D (signame); 
      far = MRI_FLOAT_PTR(im);
      /* Now call distance function */
      if (!thd_Adist ( in_set,
                       mask, 
                       far, im->ny,
                       &dist_set ,
                       oc)) {
         ERROR_exit("Failed in thd_Acluster");                 
      }
      if (im) mri_free(im); im = NULL; far = NULL; 
      /* add history to output data and write them  out */
      if( oc.verb && dist_set) 
            ININFO_message("\nWriting datasets: %s",prefix) ;
      if (dist_set) {
         EDIT_dset_items(  dist_set , ADN_prefix  , prefix, ADN_none);
         tross_Copy_History( in_set , dist_set ) ;
         tross_Make_History( "3dAclustering" , argc, argv , dist_set ) ;
         DSET_write(dist_set); DSET_unload(dist_set); 
         DSET_delete(dist_set); dist_set = NULL;
      }
   } else {
      /* Doing clustering function */
      Ncoltot=0;
      /* Read in dset(s) and create D */
      for (iset = 0; iset < N_iset; ++iset) {
         if (oc.verb) fprintf(stderr,"Patience, reading %s's header, ", 
                                    filename[iset]);
         in_set = THD_open_dataset(filename[iset]);
         CHECK_OPEN_ERROR(in_set,filename[iset]) ;
         if (oc.voxdebug[0] >= 0) {
            /* setup for debugging */
            oc.voxdebug[3] = oc.voxdebug[0] + oc.voxdebug[1]*DSET_NX(in_set) +
                              oc.voxdebug[2]*DSET_NX(in_set)*DSET_NY(in_set);
         } else oc.voxdebug[3] = -1;
         if (iset == 0) {
            ncol = DSET_NVALS(in_set);
            nrow = DSET_NVOX(in_set);
            nx = DSET_NX(in_set); ny = DSET_NY(in_set); nz = DSET_NZ(in_set);
            if (  mask                                              &&
                  ( ( (mnx >= 0 && mnx != DSET_NX(in_set)) || 
                      (mny >= 0 && mny != DSET_NY(in_set)) || 
                      (mnz >= 0 && mnz != DSET_NZ(in_set))  )    ||
                    ( mnxyz != nx*ny*nz ) ) ) {
               ERROR_exit("Dimension mismatch between mask and input dset");      
            }
            if (!mask) nmask = DSET_NVOX(in_set);
         } else { /* check for consistency with previous input */
            if (  (nx != DSET_NX(in_set) || 
                   ny != DSET_NY(in_set) || 
                   nz != DSET_NZ(in_set) ) ) {
               ERROR_exit( "Dimension mismatch between input dset"
                           " %s and preceding ones", filename[iset]);      
            }     
            ncol = DSET_NVALS(in_set);
         }
         if (oc.verb) fprintf(stderr," %d cols\n ", 
                                    ncol);
         Ncoltot += ncol;
         /* get rid of dset */
         DSET_delete (in_set);
      }

      /* Now allocate for D */
      D = (float **)calloc(sizeof(float*), nmask);
      for (ii=0;ii<(nmask);++ii) {
         if (!(D[ii] = (float *)calloc(sizeof(float), Ncoltot))) {
            fprintf(stderr,"ERROR: Failed while allocating %dx%d float matrix\n", 
                           nmask, Ncoltot);
            RETURN(1);
         }
      }

      dvec = (float * )malloc(sizeof(float)*Ncoltot) ;  /* array to hold series 
                                                         longer than needed, but 
                                                         less hassle*/
      nc0 = 0;
      for (iset = 0; iset < N_iset; ++iset) {
         if (oc.verb) 
            fprintf(stderr,"Patience, rereading %s...\n", filename[iset]);
         in_set = THD_open_dataset(filename[iset]);
         DSET_load(in_set) ; ncol = DSET_NVALS(in_set);

         if (oc.verb) {
            ININFO_message("Filling cols [%d..%d] of D(%dx%d) (mask=%p).\n", 
                              nc0,nc0+ncol-1, nmask, Ncoltot, mask);
         }
         ii = 0;
         for (nl=0; nl<DSET_NVOX(in_set); ++nl) {
            if (!mask || mask[nl]) {
               THD_extract_array( nl , in_set , 0 , dvec ) ; 
               for (nc=0; nc<ncol; ++nc) D[ii][nc0+nc] = dvec[nc]; 
               ++ii;                              
            }
         }
         nc0 += ncol;
         if (iset != N_iset-1) DSET_delete(in_set);
         else DSET_unload(in_set);
      }
      free(dvec); dvec = NULL;

      /* Now call clustering function */
      if (!thd_Acluster ( in_set,
                        mask, nmask,
                        &clust_set,
                        &dist_set ,
                        oc, D, Ncoltot)) {
         ERROR_exit("Failed in thd_Acluster");                 
      }

      /* freedom */
      if (D) {
         for (ii=0; ii<nmask; ++ii) if (D[ii]) free(D[ii]);
         free(D); D = NULL;
      }
      /* avovk; make prefix for other datasets, based on input prefix */

      n = 1 + strlen(prefix) + strlen("_vcd");
      prefixvcd = (char *)malloc(n*sizeof(char));
      sprintf (prefixvcd, "%s_vcd", prefix);


      /* add history to output data and write them  out */
      if( oc.verb && 
          (clust_set || dist_set)) 
        ININFO_message("\nWriting dataset: %s %s",prefix, prefixvcd) ;
      if (clust_set) {
         EDIT_dset_items(  clust_set , ADN_prefix  , prefix, ADN_none);
         tross_Copy_History( in_set , clust_set ) ;
         tross_Make_History( "3dAclustering" , argc, argv , clust_set ) ;
         DSET_write(clust_set); DSET_unload(clust_set); 
         DSET_delete(clust_set); clust_set = NULL;
      }
      ININFO_message("\nWriting dataset: %s", prefixvcd) ;
      if (dist_set) {
         EDIT_dset_items(  dist_set , ADN_prefix  , prefixvcd, ADN_none);
         tross_Copy_History( in_set , dist_set ) ;
         tross_Make_History( "3dAclustering" , argc, argv , dist_set ) ;
         DSET_write(dist_set); 
         ININFO_message("is it...");
         DSET_unload(dist_set); 
         ININFO_message("or is it here...");
         DSET_delete(dist_set); 
         ININFO_message("or at the end...");
         dist_set = NULL;
         ININFO_message("end.");
      }

   }
 
   
   if (mask) free(mask); mask = NULL;
   if (oc.jobname) free(oc.jobname); oc.jobname = NULL;
   

   fprintf (stderr,"\n");
   RETURN(0);
   }

