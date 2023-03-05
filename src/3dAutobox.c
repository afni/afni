#include "mrilib.h"

/*
  [PT: Oct 15, 2018] 
  + added '-extent_ijk_to_file FF' option to get slice numbers of
    auto-bboxing in a nice text file.

  [PT: Oct 15, 2018] 
  + change where+how input dset check occurs, so subbrick selection is
    possible

  [PT: Oct 15, 2018] 
  + added a couple other slice info things: 
    '-extent_ijk' to put bounding box slices to screen
    '-extent_ijk_midslice' to put midslice of bounding box to screen

  [PT: Jan 17, 2023]
  + added a couple other slice info things: 
    '-extent_xyz_to_file' to put bounding box XYZ coords to file
  + removed this note, because it does not appear to be the case (anymore?)
     "                 Also note that this value is calculated before\n"
     "                 any '-npad ...' option, so it would ignore that.\n"
  + make help file formatting a bit more readable

 */


/*
  A function that takes a dataset as input, and based on its
  orientation outputs a string of 'i', 'j' and 'k' values. For
  example, the orientation 'RPI' -> 'ijk', and 'AIL' -> 'jki'.
*/
int THD_fill_char_int_3_ijk(THD_dataxes * daxes, char ostr[4]);

int THD_fill_char_int_3_ijk(THD_dataxes * daxes, char ostr[4])
{
   int nn;
   char *ref_str = "ijk\0";

   if ( ! daxes || ! ostr ) return 1;

   /*
   INFO_message("TEST1: %d %d %d", 
                ORIENT_xyzint[daxes->xxorient]-1, 
                ORIENT_xyzint[daxes->yyorient]-1,
                ORIENT_xyzint[daxes->zzorient]-1);
   INFO_message("TEST1: %c %c %c",
                ref_str[ORIENT_xyzint[daxes->xxorient]-1 ], 
                ref_str[ORIENT_xyzint[daxes->yyorient]-1 ], 
                ref_str[ORIENT_xyzint[daxes->zzorient]-1 ]);
   */

   ostr[0] = ref_str[ORIENT_xyzint[daxes->xxorient]-1 ];
   ostr[1] = ref_str[ORIENT_xyzint[daxes->yyorient]-1 ];
   ostr[2] = ref_str[ORIENT_xyzint[daxes->zzorient]-1 ];

   return 0;
};

void help_autobox()
{
   printf(
    "Usage: 3dAutobox [options] DATASET\n"
     "Computes size of a box that fits around the volume.\n"
     "Also can be used to crop the volume to that box.\n"
     "\n"
     "The default 'info message'-based terminal text is a set of IJK coords.\n"
     "See below for options to display coordinates in other ways, as well as\n"
     "to save them in a text file.  Please note in particular the difference\n"
     "between *ijk* and *ijkord* outputs, for scripting.\n"
     "\n"
     "OPTIONS: ~1~\n"
     "\n"
     "-prefix PREFIX  :Crop the input dataset to the size of the box, and\n"
     "                 write an output dataset with PREFIX for the name.\n"
     "                 *If -prefix is not used, no new volume is written out,\n"
     "                 just the (x,y,z) extents of the voxels to be kept.\n"
     "\n"
     "-input DATASET  :An alternate way to specify the input dataset.\n"
     "                 The default method is to pass DATASET as\n"
     "                 the last parameter on the command line.\n"
     "\n"
     "-noclust        :Don't do any clustering to find box. Any non-zero\n"
     "                 voxel will be preserved in the cropped volume.\n"
     "                 The default method uses some clustering to find the\n"
     "                 cropping box, and will clip off small isolated blobs.\n"
     "\n"
     "-extent         :Write to standard out the spatial extent of the box\n"
     "\n"
     "-extent_ijk     :Write out the 6 auto bbox ijk slice numbers to\n"
     "                 screen:\n"
     "                     imin imax jmin jmax kmin kmax\n"
     "                 Note that resampling would affect the ijk vals (but\n"
     "                 not necessarily the xyz ones).\n"
     "\n"
     "-extent_ijk_to_file FF\n"
     "                :Write out the 6 auto bbox ijk slice numbers to\n"
     "                 a simple-formatted text file FF (single row file):\n"
     "                     imin imax jmin jmax kmin kmax\n"
     "                 (same notes as above apply).\n"
     "\n"
     "-extent_ijk_midslice  :Write out the 3 ijk midslices of the autobox to\n"
     "                 the screen:\n"
     "                     imid jmid kmid\n"
     "                 These are obtained via: (imin + imax)/2, etc.\n"
     "\n"
     "-extent_ijkord  :Write out the 6 auto bbox ijk slice numbers to screen\n"
     "                 but in a particular order and format (see 'NOTE on\n"
     "                 *ijkord* format', below).\n"
     "                   NB: This ordering is useful if you want to use\n"
     "                 the output indices in 3dcalc expressions.\n"
     "\n"
     "-extent_ijkord_to_file FFORRD\n"
     "                :Write out the 6 auto bbox ijk slice numbers to a file\n"
     "                  but in a particular order and format (see 'NOTE on\n"
     "                  *ijkord* format', below).\n"
     "                   NB: This option is quite useful if you want to use\n"
     "                 the output indices in 3dcalc expressions.\n"
     "\n"
     "-extent_xyz_to_file GG\n"
     "                :Write out the 6 auto bbox xyz coords to\n"
     "                 a simple-formatted text file GG (single row file):\n"
     "                     xmin xmax ymin ymax zmin zmax\n"
     "                 (same values as '-extent').\n"
     "\n"
     "-extent_xyz_midslice  :Write out the 3 xyz midslices of the autobox to\n"
     "                 the screen:\n"
     "                     xmid ymid zmid\n"
     "                 These are obtained via: (xmin + xmax)/2, etc.\n"
     "                 These follow the same meaning as '-extent'.\n"
     "\n"
     "-npad NNN       :Number of extra voxels to pad on each side of box,\n"
     "                 since some troublesome people (that's you, LRF) want\n"
     "                 this feature for no apparent reason.\n"
     "                 ** With this option, it is possible to get a dataset\n"
     "                 thatis actually bigger than the input.\n"
     "                 ** You can input a negative value for NNN, which will\n"
     "                 crop the dataset even more than the automatic method.\n"
     "\n"
     "-npad_safety_on :Constrain npad-ded extents to be within dset.  So, \n"
     "                 each index is bounded to be in range [0, L-1], where L\n"
     "                 is matrix length along that dimension.\n"
     "\n"
     "\n"
     "NOTE on *ijkord* format  ~1~\n"
     "\n"
     "Using any of the '-*ijkord*' options above will output pairs of ijk\n"
     "indices just like the regular ijk options, **but** they will be ordered\n"
     "in a way that you can associate each of the i, j, and k indices with\n"
     "a standard x, y and z coordinate direction.  Without this ordering,\n"
     "resampling a dataset could change what index is associated with which\n"
     "coordinate axis.  That situation can be confusing for scripting (and\n"
     "by confusing, we mean 'bad').\n"
     "\n"
     "The output format for any '-*ijkord*' options is a 3x3 table, where\n"
     "the first column is the index value (i, j or k), and the next two\n"
     "columns are the min and max interval boundaries for the autobox.\n"
     "Importantly, the rows are placed in order so that the top corresponds\n"
     "to the x-axis, the middle to the y-axis and the bottom to the z-axis.\n"
     "\n"
     "So, if you had the following table output for a dset:\n"
     "     k       10      170\n"
     "     i       35      254\n"
     "     j       21      199\n"
     "\n"
     "... you would look at the third row for the min/max slice values\n"
     "along the z-axis, and you would use the index 'j' to refer to it in,\n"
     "say, a 3dcalc expression.\n"
     "\n"
     "Note that the above example table output came from a dataset with ASL\n"
     "orientation.  We can see how that fits, recalling that the first,\n"
     "second and third rows tell us about x, y and z info, respectively; and\n"
     "that i, j and k refer to the first, second and third characters in the\n"
     "orientation string.  So, the third (z-like) row contains a j, which\n"
     "points us at the middle character in the orientation, which is S, which\n"
     "is along the z-axis---all consistent!  Similarly, the top (x-like) row\n"
     "contains a k, which points us at the last char in the orientation,\n"
     "which is L and that is along the x-axis---phew!\n"
     "\n"
     "The main point of this would be to extra this information and use it\n"
     "in a script.  If you knew that you wanted the z-slice range to use\n"
     "in a 3dcalc 'within()' expression, then you could extract the 3rd row\n"
     "to get the correct index and slice ranges, e.g., in tcsh:\n"
     "    set vvv = `sed -n 3p FILE_ijkord.txt`\n"
     "\n"
     "... where now ${vvv} will have 3 values, the first of which is the\n"
     "relevant index letter, then the min and max slice range values.\n"
     "So an example 3dcalc expression to keep values only within\n"
     "that slice range:\n"
     "    3dcalc                                                   \\\n"
     "        -a      DSET                                         \\\n"
     "        -expr   \"a*within(${vvv[1]},${vvv[2]},${vvv[3]})\"    \\\n"
     "        -prefix DSET_SUBSET\n"
     "\n"
   ) ;
   PRINT_COMPILE_DATE ; return ;
}




/*----------------------------------------------------------------------------*/

int main( int argc , char * argv[] )
{
   THD_3dim_dataset *dset, *outset=NULL;
   int iarg=1, npad = 0, extent=0;
   char *prefix=NULL, *iname=NULL;

   char *oijkext    = NULL;  FILE *fout_ijkext    = NULL;
   char *oxyzext    = NULL;  FILE *fout_xyzext    = NULL;
   char *oijkordext = NULL;  FILE *fout_ijkordext = NULL;
   int extent_ijk=0;
   int extent_ijk_midslice=0;
   int extent_ijkord=0;
   int imid=0, jmid=0, kmid=0;
   int extent_xyz_midslice=0;
   float xmid=0, ymid=0, zmid=0;

   char *full_orient = NULL;

   /* PT: default output ordering of IJK depends on the dset
      orientation, so we don't actually which indices go along the
      z-direction, like if we wanted to make a 3dcalc expression using
      'k' to select rows.  This array contains the IJK extents ordered
      so the first pair is along the x-direction, second pair along
      the y-direction and third along z-direction.  Used with the
      "-*ijkord*" opts
   */
   int ijk_as_rai[6] = {-1, -1, -1, -1, -1};
   char ijk_order[4] = "---\0";

   /* PT: default npad can give indices outside dset.  Activating this
      flag with the '-npad_safety_on' opt will constrain each index to
      be in range [0, L-1], where L is matrix size along that dim.
   */
   int DO_NPAD_SAFELY = 0; 


   /*-- startup bureaucracy --*/

   mainENTRY("3dAutobox main"); machdep(); AFNI_logger("3dAutobox",argc,argv);
   PRINT_VERSION("3dAutobox") ;

   /*-- read command line options --*/

   iarg = 1 ;
   while( iarg < argc && argv[iarg][0] == '-' ){

      if( strcmp(argv[iarg],"-help") == 0 || strcmp(argv[iarg],"-h") == 0){
         help_autobox();
         exit(0) ;
      }

      if( strcmp(argv[iarg],"-prefix") == 0 ){
         prefix = argv[++iarg] ;
         if( !THD_filename_ok(prefix) ){
            fprintf(stderr,"** 3dAutobox: Illegal string after -prefix!\n"); 
            exit(1) ;
         }
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-input") == 0 ){
         iname = argv[++iarg] ;
         // This is a bad check, because it doesn't permit subbrick
         // selection!  Will do check later.
         /*if( !THD_filename_ok(iname) ){
            fprintf(stderr,"** 3dAutobox: Illegal string after -input!\n"); 
            exit(1) ;
            }*/
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-noclust") == 0 ){
         MRI_autobbox_clust(0) ;  /* turn of clustering and clipping */
         THD_autobbox_clip(0) ;
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-npad") == 0 ){
        npad = (int)strtod(argv[++iarg],NULL) ;
        iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-npad_safety_on") == 0 ){
         DO_NPAD_SAFELY = 1;
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-extent_ijk") == 0 ){
        extent_ijk = 1 ;
        iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-extent_ijk_to_file") == 0 ){
			if( ++iarg >= argc ) 
				ERROR_exit("Need argument after '-extent_ijk_to_file'\n") ;
         oijkext = argv[iarg];
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-extent_ijkord") == 0 ){
        extent_ijkord = 1 ;
        iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-extent_ijkord_to_file") == 0 ){
			if( ++iarg >= argc ) 
				ERROR_exit("Need argument after '-extent_ijkord_to_file'\n") ;
         oijkordext = argv[iarg];
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-extent_xyz_to_file") == 0 ){
			if( ++iarg >= argc ) 
				ERROR_exit("Need argument after '-extent_xyz_to_file'\n") ;
         oxyzext = argv[iarg];
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-extent_ijk_midslice") == 0 ){
        extent_ijk_midslice = 1 ;
        iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-extent_xyz_midslice") == 0 ){
        extent_xyz_midslice = 1 ;
        iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-extent") == 0 ){
        extent = 1 ;
        iarg++ ; continue ;
      }

     /*- washappenin, dood? -*/

      ERROR_message("** 3dAutobox: %s makes no sense here.\n",
                 argv[iarg]) ;
      suggest_best_prog_option(argv[0], argv[iarg]);
      exit(1);
   }

   if( argc < 2){ help_autobox(); exit(0); }

   /* got input ? */

   if( iarg == argc-1 )
     iname = argv[iarg] ;
   else if (iarg != argc)
     ERROR_exit("** 3dAutobox: %s is nonsense on the line \n"
                "   I know you're John; stop pretending you have an accent!",
                argv[iarg]) ;

   if( !iname )
     ERROR_exit("** 3dAutobox: Where is my input?") ;

   /*-- read data --*/

   dset = THD_open_dataset(iname); 
   // Check here instead of after -input.
   if( dset == NULL )
      ERROR_exit("Can't open time series dataset '%s'.",iname);
   CHECK_OPEN_ERROR(dset,iname);

   if( DSET_BRICK_TYPE(dset,0) != MRI_short &&
       DSET_BRICK_TYPE(dset,0) != MRI_byte  &&
       DSET_BRICK_TYPE(dset,0) != MRI_float   )
       ERROR_exit("** ILLEGAL dataset type: %s :-(",
                  MRI_type_name[DSET_BRICK_TYPE(dset,0)]) ;

   DSET_load(dset) ; CHECK_LOAD_ERROR(dset) ;

   
   {
      int nx=DSET_NX(dset), ny=DSET_NY(dset), nz=DSET_NZ(dset), nxy=nx*ny ;
      int xm=-1,xp=-1,ym=-1,yp=-1,zm=-1,zp=-1;
      THD_autobbox( dset , &xm,&xp , &ym,&yp , &zm,&zp, NULL ) ;

      xm -= npad; ym -= npad; zm -= npad;  /* for LRF */
      xp += npad; yp += npad; zp += npad;

      /* Constrain each index to be in [0, L-1], where L is matr len.
         Some checks here might be extraneous but... they are all valid
       */
      if ( DO_NPAD_SAFELY ) {
         if ( xm < 0 )            xm = 0;
         if ( xm >= nx )          xm = nx - 1;
         if ( xp < 0 )            xp = 0;
         if ( xp >= nx )          xp = nx - 1;

         if ( ym < 0 )            ym = 0;
         if ( ym >= ny )          ym = ny - 1;
         if ( yp < 0 )            yp = 0;
         if ( yp >= ny )          yp = ny - 1;

         if ( zm < 0 )            zm = 0;
         if ( zm >= nz )          zm = nz - 1;
         if ( zp < 0 )            zp = 0;
         if ( zp >= nz )          zp = nz - 1;
      }

      INFO_message("Auto bbox: x=%d..%d  y=%d..%d  z=%d..%d\n",
                   xm,xp,ym,yp,zm,zp ) ;

      // [PT: Jan 17, 2023] do the work for ijkord, including getting
      // the associated string values
      {
         int i;
         int axes_ord[3] = {-10, -10, -10};
         char test_str[] = "---\0";

         // get int orient codes
         i = THD_fill_orient_int_3_rlpais(dset->daxes, axes_ord);
         // ... and convert the codes, ordering for x (=0), y (=1) and z (=2)
         for( i=0 ; i<3 ; i++ )
            axes_ord[i] = ORIENT_xyzint[axes_ord[i]] - 1;

         // place the paired ijk values into xyz-like order
         ijk_as_rai[2*axes_ord[0]]   = xm;
         ijk_as_rai[2*axes_ord[0]+1] = xp;

         ijk_as_rai[2*axes_ord[1]]   = ym;
         ijk_as_rai[2*axes_ord[1]+1] = yp;

         ijk_as_rai[2*axes_ord[2]]   = zm;
         ijk_as_rai[2*axes_ord[2]+1] = zp;

         THD_fill_char_int_3_ijk(dset->daxes, test_str);
         //INFO_message("TEST: %s", test_str);

         for( i=0 ; i<3 ; i++ )
            ijk_order[i] = test_str[axes_ord[i]];
         //INFO_message("TESTb: %s", ijk_order);
                  

         //INFO_message("Auto bbox: iord=%d..%d  jord=%d..%d kord=%d..%d\n",
         //             ijk_as_rai[0], ijk_as_rai[1], ijk_as_rai[2], 
         //             ijk_as_rai[3], ijk_as_rai[4], ijk_as_rai[5] );
      }

      // [PT: Oct 18, 2018] New output text file, if desired
      if( oijkext ) {
         if( (fout_ijkext = fopen(oijkext, "w")) == NULL ) {
            fprintf(stderr, "\n\nError opening file %s.\n", oijkext);
            exit(1);
         }
         fprintf( fout_ijkext, "%8d %8d %8d %8d %8d %8d\n",
                  xm, xp, ym, yp, zm, zp );
         fclose(fout_ijkext);
         INFO_message("Wrote ijk extents file: %s", oijkext);
      }

      if( extent_ijk ) 
         printf( "%8d %8d %8d %8d %8d %8d\n",
                 xm, xp, ym, yp, zm, zp );

      if( extent_ijk_midslice ) {
         imid = (xm + xp) / 2;  // integer division fine, b/c we need ints
         jmid = (ym + yp) / 2;
         kmid = (zm + zp) / 2;
         printf( "%8d %8d %8d\n", imid, jmid, kmid );
      }

      if( extent_ijkord ) 
         printf( "%c %8d %8d\n%c %8d %8d\n%c %8d %8d\n",
                 ijk_order[0], ijk_as_rai[0], ijk_as_rai[1], 
                 ijk_order[1], ijk_as_rai[2], ijk_as_rai[3], 
                 ijk_order[2], ijk_as_rai[4], ijk_as_rai[5] );

      if ( (extent && !prefix) || (extent_xyz_midslice && !prefix) || \
           (oxyzext && !prefix) || (oijkordext && !prefix) )
         prefix = "EXTENT_ONLY";

      if( prefix ){
         outset = THD_zeropad( dset ,
                               -xm, xp-nx+1,
                               -ym, yp-ny+1,
                               -zm, zp-nz+1,
                               prefix , ZPAD_IJK ) ;
         if( THD_deathcon() && THD_is_file(DSET_HEADNAME(outset)) )
            ERROR_exit("3dAutobox: output file %s already exists :-(",
                       DSET_HEADNAME(outset) ) ;

         if( outset == NULL )
            ERROR_exit("3dAutobox: Some error occurred in processing :-(") ;

         tross_Copy_History( dset , outset ) ;       /* 31 Jan 2001 - RWCox */
         tross_Make_History( "3dAutobox" , argc,argv , outset ) ;

         if (!strstr(prefix,"EXTENT_ONLY")) {
            DSET_write(outset) ;
            INFO_message("3dAutobox: output dataset = %s",
                         DSET_BRIKNAME(outset)) ;
         }
         if (extent) {
          float RL_AP_IS[6];
          THD_dset_extent(outset, '-', RL_AP_IS);
          printf("Extent auto bbox: R=%f L=%f  A=%f P=%f  I=%f S=%f\n",
                    RL_AP_IS[0],RL_AP_IS[1],
                    RL_AP_IS[2],RL_AP_IS[3],
                    RL_AP_IS[4],RL_AP_IS[5] ) ;
         }

         // [PT: Jan 17, 2023] New output text file: xyz
         if( oxyzext ) {
            float RL_AP_IS[6];
            THD_dset_extent(outset, '-', RL_AP_IS);
            if( (fout_xyzext = fopen(oxyzext, "w")) == NULL ) {
               fprintf(stderr, "\n\nError opening file %s.\n", oxyzext);
               exit(1);
            }
            fprintf( fout_xyzext, "%f  %f  %f  %f  %f  %f\n",
                     RL_AP_IS[0], RL_AP_IS[1],
                     RL_AP_IS[2], RL_AP_IS[3],
                     RL_AP_IS[4], RL_AP_IS[5] );
            fclose(fout_xyzext);
            INFO_message("Wrote xyz extents file: %s", oxyzext);
         }

         // [PT: Jan 17, 2023] New output text file: ijkord
         if( oijkordext ) {
            if( (fout_ijkordext = fopen(oijkordext, "w")) == NULL ) {
               fprintf(stderr, "\n\nError opening file %s.\n", oijkordext);
               exit(1);
            }
            fprintf( fout_ijkordext, 
                     "%c %8d %8d\n%c %8d %8d\n%c %8d %8d\n",
                     ijk_order[0], ijk_as_rai[0], ijk_as_rai[1], 
                     ijk_order[1], ijk_as_rai[2], ijk_as_rai[3], 
                     ijk_order[2], ijk_as_rai[4], ijk_as_rai[5] );
            fclose(fout_ijkordext);
            INFO_message("Wrote ijkord extents file: %s", oijkordext);
         }

         if( extent_xyz_midslice ) {
            INFO_message("aaa" );
            float RL_AP_IS2[6];
            THD_dset_extent(outset, '-', RL_AP_IS2);
            xmid = (RL_AP_IS2[0] + RL_AP_IS2[1]) / 2.;
            ymid = (RL_AP_IS2[2] + RL_AP_IS2[3]) / 2.;
            zmid = (RL_AP_IS2[4] + RL_AP_IS2[5]) / 2.;
            printf( "%10.5f %10.5f %10.5f\n", xmid, ymid, zmid );
         }
      }
   }

   exit(0) ;
}
