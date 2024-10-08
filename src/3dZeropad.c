#include "mrilib.h"
void usage_3dZeropad(int detail)
{
         printf("Usage: 3dZeropad [options] dataset ~1~\n"
 "** Adds planes of zeros to a dataset (i.e., pads it out).\n"
 "** A negative 'add' count means to cut a dataset down in size.\n"
 "   [Remember 3rd grade arithmetic, please.]\n"
 "\n"
 "Options: ~2~\n"
 "  -I n = adds 'n' planes of zero at the Inferior edge\n"
 "  -S n = adds 'n' planes of zero at the Superior edge\n"
 "  -A n = adds 'n' planes of zero at the Anterior edge\n"
 "  -P n = adds 'n' planes of zero at the Posterior edge\n"
 "  -L n = adds 'n' planes of zero at the Left edge\n"
 "  -R n = adds 'n' planes of zero at the Right edge\n"
 "  -z n = adds 'n' planes of zeros on EACH of the\n"
 "          dataset z-axis (slice-direction) faces\n"
 "\n"
 " -RL a = These options specify that planes should be added/cut\n"
 " -AP b = symmetrically to make the resulting volume have\n"
 " -IS c = 'a', 'b', and 'c' slices in the respective directions.\n"
 "\n"
 " -pad2odds = add 0 or 1 plane in each of the R/A/S directions,\n"
 "              giving each axis an odd number of slices\n"
 " -pad2evens = add 0 or 1 plane in each of the R/A/S directions,\n"
 "              giving each axis an even number of slices\n"
 " -pad2mult N = add planes in each of the R/A/S directions,\n"
 "              making each number of planes a multiple of N\n"
 "     NB: for the -pad* opts, any padding will be applied on a side\n"
 "         *away* from the coordinate origin. So, if the dset is RAI,\n"
 "         padding would be applied to L and/or P and/or S sides.\n"
 "\n"
 " -mm   = pad counts 'n' are in mm instead of slices:\n"
 "         * each 'n' is an integer\n"
 "         * at least 'n' mm of slices will be added/removed:\n"
 "            n =  3 and slice thickness = 2.5 mm ==> 2 slices added\n"
 "            n = -6 and slice thickness = 2.5 mm ==> 3 slices removed\n"
 "\n"
 " -master mset = match the volume described in dataset 'mset':\n"
 "                * mset must have the same orientation and grid\n"
 "                   spacing as dataset to be padded\n"
 "                * the goal of -master is to make the output dataset\n"
 "                   from 3dZeropad match the spatial 'extents' of\n"
 "                   mset (cf. 3dinfo output) as much as possible,\n"
 "                   by adding/subtracting slices as needed.\n"
 "                * you can't use -I,-S,..., or -mm with -master\n"
 "\n"
 " -prefix ppp = write result into dataset with prefix 'ppp'\n"
 "                 [default = 'zeropad']\n"
 "\n"
 "Nota Bene: ~1~\n"
 " * You can use negative values of n to cut planes off the edges\n"
 "     of a dataset.  At least one plane must be added/removed\n"
 "     or the program won't do anything.\n"
 " * Anat parent and Talairach markers are NOT preserved in the\n"
 "     new dataset.\n"
#if 0
 " * If the old dataset has z-slice-dependent time offsets, and\n"
 "     if new z-planes are added, all the slice-dependent time\n"
 "     offsets will be removed.\n"
#else
 " * If the old dataset has z-slice-dependent time offsets, and\n"
 "     if new (zero filled) z-planes are added, the time offsets\n"
 "     of the new slices will be set to zero.\n"
#endif
 " * You can use program '3dinfo' to find out how many planes\n"
 "     a dataset has in each direction.\n"
 " * Program works for byte-, short-, float-, and complex-valued\n"
 "     datasets.\n"
 " * You can use a sub-brick selector on the input dataset.\n"
 "\n"
 " Author: RWCox - July 2000\n"
           ) ;
      PRINT_COMPILE_DATE ; return ;

}

/* Function to take pad2mult int and figure out how many slices need
  to be added along each axis, and which side to apply it to. The rule
  for the latter is that it will apply it to slices *away* from the
  header origin. So, if the DSET is RAI, AIR, RAI, etc, then slices
  would be added only to L and/or P and/or S sides.

  This function is longer than necessary, because it calculates stuff
  to report in the terminal.

  Inputs
  ------
  inset    : name of dset
  pad2mult : the integer which must be a factor of the new, padded
             matrix
  add_R    : number of slices to add in R direction
  add_L    : number of slices to add in L direction
  [etc for other add_?]

  Returns
  -------
  simply an integer with successful completion.
  NB: the main 'output' is editing the values of add_? that were input
*/
int get_pad2mult_padding(THD_3dim_dataset *inset, int pad2mult,
                         int *add_R, int *add_L, 
                         int *add_A, int *add_P, 
                         int *add_I, int *add_S);
int get_pad2mult_padding(THD_3dim_dataset *inset, int pad2mult,
                         int *add_R, int *add_L, 
                         int *add_A, int *add_P, 
                         int *add_I, int *add_S)
{
   THD_dataxes * iax = inset->daxes ;
   int           ndrl, ndap, ndis ;
   char          ori[4] = "   " ;
   int           ii = 0 ;
   int           rep_RL, rep_AP, rep_IS ;
   char          rep_sli[4] = "LPS"; // def sides to add to (-> RAI dset)

   // nvox along each axis
   ndrl = DAXES_NUM(iax,ORI_R2L_TYPE) ;
   ndap = DAXES_NUM(iax,ORI_A2P_TYPE) ;
   ndis = DAXES_NUM(iax,ORI_I2S_TYPE) ;

   // store header origin
   ori[0]=ORIENT_typestr[iax->xxorient][0];
   ori[1]=ORIENT_typestr[iax->yyorient][0];
   ori[2]=ORIENT_typestr[iax->zzorient][0];
   ori[3]='\0';

   /* pad to odd: below add_L might switch to add_R, etc. */
   if( pad2mult == 1 ) {
      *add_L = 1-(ndrl % 2) ; /* same as 2even, but "negate" */
      *add_P = 1-(ndap % 2) ;
      *add_S = 1-(ndis % 2) ;
   } else if ( pad2mult == 2 ) {
      *add_L = ndrl % 2 ; /* add 1 if odd */
      *add_P = ndap % 2 ;
      *add_S = ndis % 2 ;
   } else {
      /* otherwise, we want to add the negative mod nvoxels
         (but -v1 % m is negative, grrrrrr...) */
      *add_L = (pad2mult - (ndrl % pad2mult)) % pad2mult;
      *add_P = (pad2mult - (ndap % pad2mult)) % pad2mult;
      *add_S = (pad2mult - (ndis % pad2mult)) % pad2mult;
   }

   /* store padding, for reporting below */
   rep_RL = *add_L;
   rep_AP = *add_P;
   rep_IS = *add_S;

   /* Check about switching add_L to add_R, etc.  That is, ensure that
      if padding is done along a particular axis, that it is done on
      side *away* from the matrix origin, by swapping the add_? value
      if it is nonzero.
   */
   for ( ii=0 ; ii<3 ; ii++ )
      if ( strncmp(ori+ii, "L",1) == 0 ) {
         rep_sli[0] = 'R';
         if ( *add_L ){
            *add_R = *add_L;
            *add_L = 0;
         }
      }
   for ( ii=0 ; ii<3 ; ii++ )
      if ( strncmp(ori+ii, "P",1) == 0 ) {
         rep_sli[1] = 'A';
         if ( *add_P ){
            *add_A = *add_P;
            *add_P = 0;
         }
      }
   for ( ii=0 ; ii<3 ; ii++ )
      if ( strncmp(ori+ii, "S",1) == 0 ) {
         rep_sli[2] = 'I';
         if ( *add_S ){
            *add_I = *add_S;
            *add_S = 0;
         }
      }

   INFO_message("pad2mult %d: orient = %s, applying -%c %d -%c %d -%c %d\n",
                pad2mult, ori, rep_sli[0], rep_RL, 
                rep_sli[1], rep_AP, 
                rep_sli[2], rep_IS);
   return 0;
}


int main( int argc , char * argv[] )
{
   int iarg ;
   THD_3dim_dataset *inset , *outset ;
   int add_I=0 , add_S=0 , add_A=0 , add_P=0 , add_L=0 , add_R=0 ;
   int RLsiz=0, APsiz=0, ISsiz=0 ; /* 23 Mar 2004 */
   int add_any=0 ;
   /* was pad2evens, now allow none, evens, odds, mult_N = 0,1,2,3... */
   int pad2mult=0; /* 18 Jan 2024 [rickr] */
   char * prefix="zeropad" ;

   int add_z=0 ;   /* 07 Feb 2001 */
   int mm_flag=0 ; /* 13 Feb 2001 */
   int flag ;
   int check=0; 

   THD_3dim_dataset *mset=NULL ; /* 14 May 2002 */

   /*-- help? --*/
   mainENTRY("3dZeropad main"); machdep(); AFNI_logger("3dZeropad",argc,argv);
   PRINT_VERSION("3dZeropad") ;

   /*-- read command line options --*/
   if( argc == 1){ usage_3dZeropad(1); exit(0); } /* Bob's help shortcut */

   iarg = 1 ;
   while( iarg < argc && argv[iarg][0] == '-' ){

      if( strcmp(argv[iarg],"-help") == 0 || strcmp(argv[iarg],"-h") == 0){
         usage_3dZeropad(strlen(argv[iarg])>3 ? 2:1);
         exit(0);
      }

      /*- -I, -S, etc. -*/

      if( strlen(argv[iarg]) == 2 ){
         switch( argv[iarg][1] ){
            case 'I': add_I = (int) strtod(argv[++iarg],NULL) ; break ;
            case 'S': add_S = (int) strtod(argv[++iarg],NULL) ; break ;
            case 'A': add_A = (int) strtod(argv[++iarg],NULL) ; break ;
            case 'P': add_P = (int) strtod(argv[++iarg],NULL) ; break ;
            case 'L': add_L = (int) strtod(argv[++iarg],NULL) ; break ;
            case 'R': add_R = (int) strtod(argv[++iarg],NULL) ; break ;

            /* 07 Feb 2001: slice-direction is special */

            case 'z':
            case 'Z': add_z = (int) strtod(argv[++iarg],NULL) ; break ;

            default:
               fprintf(stderr,"** 3dZeropad: Illegal option: %s\n",argv[iarg]) ; exit(1) ;
         }

         if( mset != NULL ){
           fprintf(stderr,"** 3dZeropad: Can't use %s with -master!\n",argv[iarg-1]) ;
           exit(1) ;
         }

         iarg++ ; continue ;  /* skip to next argument */
      }

      /*- -RL, -AP, -IS [23 Mar 2004] -*/

      if( strcmp(argv[iarg],"-RL") == 0 || strcmp(argv[iarg],"-LR") == 0 ){
        if( add_R || add_L || mset != NULL ){
          fprintf(stderr,"** 3dZeropad: Can't use -RL with -R, -L, or -master!\n");
          exit(1) ;
        }
        RLsiz = (int) strtod(argv[++iarg],NULL) ;
        if( RLsiz < 1 ){
          fprintf(stderr,"** 3dZeropad: value after -RL is illegal!\n") ;
          exit(1) ;
        }
        iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-AP") == 0 || strcmp(argv[iarg],"-PA") == 0 ){
        if( add_A || add_P || mset != NULL ){
          fprintf(stderr,"** 3dZeropad: Can't use -AP with -A, -P, or -master!\n");
          exit(1) ;
        }
        APsiz = (int) strtod(argv[++iarg],NULL) ;
        if( APsiz < 1 ){
          fprintf(stderr,"** 3dZeropad: value after -AP is illegal!\n") ;
          exit(1) ;
        }
        iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-IS") == 0 || strcmp(argv[iarg],"-SI") == 0 ){
        if( add_S || add_I || mset != NULL ){
          fprintf(stderr,"** 3dZeropad: Can't use -IS with -I, -S, or -master!\n");
          exit(1) ;
        }
        ISsiz = (int) strtod(argv[++iarg],NULL) ;
        if( ISsiz < 1 ){
          fprintf(stderr,"** 3dZeropad: value after -IS is illegal!\n") ;
          exit(1) ;
        }
        iarg++ ; continue ;
      }

      /*- -mm -*/

      if( strcmp(argv[iarg],"-mm") == 0 ){
         if( mset != NULL ){
           fprintf(stderr,"** 3dZeropad: Can't use %s with -master!\n",argv[iarg]) ;
           exit(1) ;
         }
         mm_flag = 1 ;
         iarg++ ; continue ;
      }

      /*- -pad2* - handle many versions of this [18 Jan 2024 rickr] -*/
      /* todo: add -pad2mult N */

      if( strcmp(argv[iarg],"-pad2odds") == 0 ){
         pad2mult = 1 ;
         iarg++ ; continue ;
      }
      if( strcmp(argv[iarg],"-pad2evens") == 0 ){
         pad2mult = 2 ;
         iarg++ ; continue ;
      }
      if( strcmp(argv[iarg],"-pad2mult") == 0 ){
         int pad=0;
         pad = (int)strtod(argv[++iarg],NULL) ;
         if( pad <= 0 )
            ERROR_exit("illegal pad2mult %d (from %s)", pad, argv[iarg]);
	
         pad2mult = pad ;
         iarg++ ; continue ;
      }

      /*- -prefix -*/

      if( strcmp(argv[iarg],"-prefix") == 0 ){
         prefix = argv[++iarg] ;
         if( !THD_filename_ok(prefix) ){
            fprintf(stderr,"** 3dZeropad: Illegal string after -prefix!\n"); exit(1) ;
         }
         iarg++ ; continue ;
      }

      /*-- -master [14 May 2002] --*/

      if( strcmp(argv[iarg],"-master") == 0 ){
        if( add_I || add_S || add_A || mm_flag ||
            add_P || add_R || add_L || add_z   ||
            RLsiz || APsiz || ISsiz              ){

          fprintf(stderr,"** 3dZeropad: Can't use -master with -I,-S,-A,-P,-R,-L, or -mm!\n");
          exit(1) ;
        }
        if( mset != NULL ){
          fprintf(stderr,"** 3dZeropad: Can't use -master twice!\n"); exit(1);
        }

        mset = THD_open_dataset( argv[++iarg] ) ;
        if( !ISVALID_DSET(mset) ){
          fprintf(stderr,"** 3dZeropad: Can't open -master %s\n",argv[iarg]); exit(1);
        }
        THD_make_cardinal(mset);    /* deoblique    21 Oct, 2011 [rickr] */
         iarg++ ; continue ;
      }

      /*- what the hell? -*/

      fprintf(stderr,"** 3dZeropad: Illegal option: %s\n",argv[iarg]) ;
      suggest_best_prog_option(argv[0], argv[iarg]);
      exit(1) ;
   }

   if (iarg < 2) {
      ERROR_message("Too few options, try %s -help for details\n",argv[0]);
      exit(1);
   }

   /*- check to see if the user asked for something, anything -*/

   add_any =  add_I || add_S || add_P || add_A || add_L || add_R
                    || add_z || RLsiz || APsiz || ISsiz ;

   /* pad2mult should be used alone */
   if( pad2mult && (add_any || mset) )
      ERROR_exit("Cannot combine -pad2* with other padding or master") ;

   if( mset == NULL && add_any == 0 && pad2mult == 0 ) {
      fprintf(stderr,"++ 3dZeropad: All inputs are zero? Making a copy!\n") ;
   }

   /* check for conflicts [23 Mar 2004] */

   if( RLsiz > 0 && (add_R || add_L || add_z) ){
     fprintf(stderr,"** 3dZeropad: Can't use -R or -L or -z with -RL!\n"); exit(1);
   }
   if( APsiz > 0 && (add_A || add_P || add_z) ){
     fprintf(stderr,"** 3dZeropad: Can't use -A or -P or -z with -AP!\n"); exit(1);
   }
   if( ISsiz > 0 && (add_I || add_S || add_z) ){
     fprintf(stderr,"** 3dZeropad: Can't use -I or -S or -z with -IS!\n"); exit(1);
   }

   /*-- read the input dataset --*/

   if( iarg >= argc ){
      fprintf(stderr,"** 3dZeropad: No input dataset on command line!\n"); exit(1);
   }

#if 0
   if( strncmp(argv[iarg],"3dcalc(",7) == 0 ){
      fprintf(stderr,"** 3dZeropad: Can't use '3dcalc()' input datasets here!\n"); exit(1);
   }
#endif

   inset = THD_open_dataset( argv[iarg] ) ;
   if( inset == NULL ){
      fprintf(stderr,"** 3dZeropad: Can't open dataset %s\n",argv[iarg]); exit(1);
   }
   THD_make_cardinal(inset);    /* deoblique    21 Oct, 2011 [rickr] */

#if 0
   if( DSET_IS_MASTERED(inset) ){
      fprintf(stderr,"** 3dZeropad: Can't use partial datasets!\n"); exit(1);
   }
#endif

   /*-- if pad2mult, decide on padding         [23 Oct 2019 rickr] --*/
   /* do not worry about nx,ny,nz, focus on RL/AP,IS                 */
   /* [PT: 27 Sep 2024] move pad2mult work to a function             */
   if( pad2mult ) 
      check = get_pad2mult_padding(inset, pad2mult, &add_R, &add_L, 
                                   &add_A, &add_P, &add_I, &add_S);

   /*-- 14 May 2002: use master dataset now? --*/

   if( mset != NULL ){
     THD_dataxes *max=mset->daxes, *iax=inset->daxes ;
     int nerr=0 ;
     float mxbot,mybot,mzbot , mxtop,mytop,mztop , mdx,mdy,mdz ;
     float ixbot,iybot,izbot , ixtop,iytop,iztop , idx,idy,idz ;
     int   mnx,mny,mnz , inx,iny,inz ;
     int   add_xb,add_xt , add_yb,add_yt , add_zb,add_zt ;

     /* check if datasets are oriented the same */

     if( max->xxorient != iax->xxorient ||
         max->yyorient != iax->yyorient ||
         max->zzorient != iax->zzorient   ){

       fprintf(stderr,
   "** 3dZeropad: Master (%s) and Input (%s) dataset not oriented the same!\n",
               DSET_PREFIX(mset), DSET_PREFIX(inset));
       nerr++ ;
     }

     /* check if datasets have same voxel dimensions */

     mdx = max->xxdel ; mdy = max->yydel ; mdz = max->zzdel ;
     idx = iax->xxdel ; idy = iax->yydel ; idz = iax->zzdel ;
     mnx = max->nxx   ; mny = max->nyy   ; mnz = max->nzz   ;
     inx = iax->nxx   ; iny = iax->nyy   ; inz = iax->nzz   ;

     if( fabs(mdx-idx) > 0.01*fabs(mdx) ||
         fabs(mdy-idy) > 0.01*fabs(mdy) ||
         fabs(mdz-idz) > 0.01*fabs(mdz)   ){

       fprintf(stderr,"** 3dZeropad: Master and Input datasets don't have same voxel size!\n");
       nerr++ ;
     }

     if( nerr ) exit(1) ;

     /* calculate coords at top and bottom of each dataset */

     mxbot = max->xxorg ; mxtop = mxbot + mnx*mdx ;
     mybot = max->yyorg ; mytop = mybot + mny*mdy ;
     mzbot = max->zzorg ; mztop = mzbot + mnz*mdz ;

     ixbot = iax->xxorg ; ixtop = ixbot + inx*idx ;
     iybot = iax->yyorg ; iytop = iybot + iny*idy ;
     izbot = iax->zzorg ; iztop = izbot + inz*idz ;

     /* calculate amount to add/trim at each face */

     add_xb = (int) rint((ixbot-mxbot)/idx) ;
     add_xt = (int) rint((mxtop-ixtop)/idx) ;
     add_yb = (int) rint((iybot-mybot)/idy) ;
     add_yt = (int) rint((mytop-iytop)/idy) ;
     add_zb = (int) rint((izbot-mzbot)/idz) ;
     add_zt = (int) rint((mztop-iztop)/idz) ;

     /* map trims from x,y,z to RL,AP,IS coords */

     switch( iax->xxorient ){
       case ORI_R2L_TYPE: add_R = add_xb ; add_L = add_xt ; break ;
       case ORI_L2R_TYPE: add_L = add_xb ; add_R = add_xt ; break ;
       case ORI_I2S_TYPE: add_I = add_xb ; add_S = add_xt ; break ;
       case ORI_S2I_TYPE: add_S = add_xb ; add_I = add_xt ; break ;
       case ORI_A2P_TYPE: add_A = add_xb ; add_P = add_xt ; break ;
       case ORI_P2A_TYPE: add_P = add_xb ; add_A = add_xt ; break ;
     }

     switch( iax->yyorient ){
       case ORI_R2L_TYPE: add_R = add_yb ; add_L = add_yt ; break ;
       case ORI_L2R_TYPE: add_L = add_yb ; add_R = add_yt ; break ;
       case ORI_I2S_TYPE: add_I = add_yb ; add_S = add_yt ; break ;
       case ORI_S2I_TYPE: add_S = add_yb ; add_I = add_yt ; break ;
       case ORI_A2P_TYPE: add_A = add_yb ; add_P = add_yt ; break ;
       case ORI_P2A_TYPE: add_P = add_yb ; add_A = add_yt ; break ;
     }

     switch( iax->zzorient ){
       case ORI_R2L_TYPE: add_R = add_zb ; add_L = add_zt ; break ;
       case ORI_L2R_TYPE: add_L = add_zb ; add_R = add_zt ; break ;
       case ORI_I2S_TYPE: add_I = add_zb ; add_S = add_zt ; break ;
       case ORI_S2I_TYPE: add_S = add_zb ; add_I = add_zt ; break ;
       case ORI_A2P_TYPE: add_A = add_zb ; add_P = add_zt ; break ;
       case ORI_P2A_TYPE: add_P = add_zb ; add_A = add_zt ; break ;
     }

     fprintf(stderr,"++ 3dZeropad -master => -I %d -S %d -A %d -P %d -R %d -L %d\n",
             add_I,add_S,add_A,add_P,add_R,add_L ) ;

     DSET_delete(mset) ;
   }

   /*-- 07 Feb 2001: if -z was used, fix that now --*/

   if( add_z != 0 ){
      switch( inset->daxes->zzorient ){
         case ORI_R2L_TYPE:
         case ORI_L2R_TYPE:
            if( add_R != 0 && add_R != add_z ) fprintf(stderr,"++ 3dZeropad: -z overrides -R\n");
            if( add_L != 0 && add_L != add_z ) fprintf(stderr,"++ 3dZeropad: -z overrides -L\n");
            add_R = add_L = add_z ;
         break ;

         case ORI_P2A_TYPE:
         case ORI_A2P_TYPE:
            if( add_P != 0 && add_P != add_z ) fprintf(stderr,"++ 3dZeropad: -z overrides -P\n");
            if( add_A != 0 && add_A != add_z ) fprintf(stderr,"++ 3dZeropad: -z overrides -A\n");
            add_P = add_A = add_z ;
         break ;

         case ORI_I2S_TYPE:
         case ORI_S2I_TYPE:
            if( add_I != 0 && add_I != add_z ) fprintf(stderr,"++ 3dZeropad: -z overrides -I\n");
            if( add_I != 0 && add_S != add_z ) fprintf(stderr,"++ 3dZeropad: -z overrides -S\n");
            add_I = add_S = add_z ;
         break ;
      }
   }

   /*-- 23 Mar 2004: expand/contract if ordered --*/

   if( RLsiz > 0 ){
     int nold=0 ;
          if( inset->daxes->xxorient == ORI_R2L_TYPE || inset->daxes->xxorient == ORI_L2R_TYPE )
       nold = inset->daxes->nxx ;
     else if( inset->daxes->yyorient == ORI_R2L_TYPE || inset->daxes->yyorient == ORI_L2R_TYPE )
       nold = inset->daxes->nyy ;
     else if( inset->daxes->zzorient == ORI_R2L_TYPE || inset->daxes->zzorient == ORI_L2R_TYPE )
       nold = inset->daxes->nzz ;
     if( nold > 0 ){
       add_R = (RLsiz-nold) / 2 ;
       add_L = RLsiz-(nold+add_R) ;
     }
   }

   if( APsiz > 0 ){
     int nold=0 ;
          if( inset->daxes->xxorient == ORI_A2P_TYPE || inset->daxes->xxorient == ORI_P2A_TYPE )
       nold = inset->daxes->nxx ;
     else if( inset->daxes->yyorient == ORI_A2P_TYPE || inset->daxes->yyorient == ORI_P2A_TYPE )
       nold = inset->daxes->nyy ;
     else if( inset->daxes->zzorient == ORI_A2P_TYPE || inset->daxes->zzorient == ORI_P2A_TYPE )
       nold = inset->daxes->nzz ;
     if( nold > 0 ){
       add_A = (APsiz-nold) / 2 ;
       add_P = APsiz-(nold+add_A) ;
     }
   }

   if( ISsiz > 0 ){
     int nold=0 ;
          if( inset->daxes->xxorient == ORI_I2S_TYPE || inset->daxes->xxorient == ORI_S2I_TYPE )
       nold = inset->daxes->nxx ;
     else if( inset->daxes->yyorient == ORI_I2S_TYPE || inset->daxes->yyorient == ORI_S2I_TYPE )
       nold = inset->daxes->nyy ;
     else if( inset->daxes->zzorient == ORI_I2S_TYPE || inset->daxes->zzorient == ORI_S2I_TYPE )
       nold = inset->daxes->nzz ;
     if( nold > 0 ){
       add_I = (ISsiz-nold) / 2 ;
       add_S = ISsiz-(nold+add_I) ;
     }
   }

   /*-- 04 Oct 2000: all the real work is now in thd_zeropad.c --*/

   flag = ZPAD_PURGE ;
   if( mm_flag ) flag |= ZPAD_MM ;

   outset = THD_zeropad( inset ,
                         add_I, add_S, add_A, add_P, add_L, add_R,
                         prefix , flag ) ;

   if( THD_deathcon() && THD_is_file(DSET_HEADNAME(outset)) ){
      fprintf(stderr,
              "** 3dZeropad: output file %s already exists - FATAL ERROR!\n",
              DSET_HEADNAME(outset) ) ;
      exit(1) ;
   }

   if( outset == NULL ){
      fprintf(stderr,"** 3dZeropad: Some error occurred in processing!\n") ;
      exit(1) ;
   }

   tross_Copy_History( inset , outset ) ;             /* 31 Jan 2001 - RWCox */
   tross_Make_History( "3dZeropad" , argc,argv , outset ) ;

   if (DSET_write(outset) != False) {
      fprintf(stderr,"++ output dataset: %s\n",DSET_BRIKNAME(outset)) ;
      exit(0) ;
   } else {
      fprintf(stderr,
              "** 3dZeropad: Failed to write output!\n" ) ;
      exit(1) ;
   }

}
