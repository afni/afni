#include "mrilib.h"

/*--------------------------------------------------------------------------
  Program to transform 3-vectors based on warps stored in AFNI .HEAD files.
  Transformations are stored in +view.HEAD files (view=acpc or tlrc) between
  the +orig and +view coordinate systems.

  24 Oct 2001: creation by RWCox
----------------------------------------------------------------------------*/

/*** Some prototypes ***/

static THD_fvec3 AFNI_forward_warp_vector ( THD_warp * , THD_fvec3 ) ;
static THD_fvec3 AFNI_backward_warp_vector( THD_warp * , THD_fvec3 ) ;

#if 0
static THD_fvec3 THD_dicomm_to_surefit( THD_3dim_dataset *, THD_fvec3 ) ;
static THD_fvec3 THD_surefit_to_dicomm( THD_3dim_dataset *, THD_fvec3 ) ;
#endif

#undef DEBUG

/*--------------------------------------------------------------------------*/

static void Syntax(void)
{
   printf(
    "Usage: Vecwarp [options]\n"
    "Transforms (warps) a list of 3-vectors into another list of 3-vectors\n"
    "according to the options.  Error messages, warnings, and informational\n"
    "messages are written to stderr.  If a fatal error occurs, the program\n"
    "exits with status 1; otherwise, it exits with status 0.\n"
    "\n"
    "OPTIONS:\n"
    " -apar aaa   = Use the AFNI dataset 'aaa' as the source of the\n"
    "               transformation; this dataset must be in +acpc\n"
    "               or +tlrc coordinates, and must contain the\n"
    "               attributes WARP_TYPE and WARP_DATA which describe\n"
    "               the forward transformation from +orig coordinates\n"
    "               to the 'aaa' coordinate system.\n"
    "             N.B.: The +orig version of this dataset must also be\n"
    "                   readable, since it is also needed when translating\n"
    "                   vectors between SureFit and AFNI coordinates.\n"
    "                   Only the .HEAD files are actually used.\n"
    "\n"
    " -matvec mmm = Read an affine transformation matrix-vector from file\n"
    "               'mmm', which must be in the format\n"
    "                   u11 u12 u13 v1\n"
    "                   u21 u22 u23 v2\n"
    "                   u31 u32 u33 v3\n"
    "               where each 'uij' and 'vi' is a number.  The forward\n"
    "               transformation is defined as\n"
    "                   [ xout ]   [ u11 u12 u13 ] [ xin ]   [ v1 ]\n"
    "                   [ yout ] = [ u21 u22 u23 ] [ yin ] + [ v2 ]\n"
    "                   [ zout ]   [ u31 u32 u33 ] [ zin ]   [ v3 ]\n"
    "\n"
    " Exactly one of -apar or -matvec must be used to specify the\n"
    " transformation.\n"
    "\n"
    " -forward    = -forward means to apply the forward transformation;\n"
    "   *OR*        -backward means to apply the backward transformation\n"
    " -backward     * For example, if the transformation is specified by\n"
    "                  '-apar fred+tlrc', then the forward transformation\n"
    "                  is from +orig to +tlrc coordinates, and the backward\n"
    "                  transformation is from +tlrc to +orig coordinates.\n"
    "               * If the transformation is specified by -matvec, then\n"
    "                  the matrix-vector read in defines the forward\n"
    "                  transform as above, and the backward transformation\n"
    "                  is defined as the inverse.\n"
    "               * If neither -forward nor -backward is given, then\n"
    "                  -forward is the default.\n"
    "\n"
    " -input iii  = Read input 3-vectors from file 'iii' (from stdin if\n"
    "               'iii' is '-' or the -input option is missing).  Input\n"
    "               data may be in one of the following ASCII formats:\n"
    "\n"
    "               * SureFit .coord files:\n"
    "                   BeginHeader\n"
    "                   lines of text ...\n"
    "                   EndHeader\n"
    "                   count\n"
    "                   int x y z\n"
    "                   int x y z\n"
    "                   et cetera...\n"
    "                 In this case, everything up to and including the\n"
    "                 count is simply passed through to the output.  Each\n"
    "                 (x,y,z) triple is transformed, and output with the\n"
    "                 int label that precedes it.  Lines that cannot be\n"
    "                 scanned as 1 int and 3 floats are treated as comments\n"
    "                 and are passed to through to the output unchanged.\n"
    "               N.B.: SureFit coordinates are\n"
    "                   x = distance Right    of Left-most      dataset corner\n"
    "                   y = distance Anterior to Posterior-most dataset corner\n"
    "                   z = distance Superior to Inferior-most  dataset corner\n"
    "                 For example, if the transformation is specified by\n"
    "                   -forward -apar fred+tlrc\n"
    "                 then the input (x,y,z) are relative to fred+orig and the\n"
    "                 output (x,y,z) are relative to fred+tlrc.  If instead\n"
    "                   -backward -apar fred+tlrc\n"
    "                 is used, then the input (x,y,z) are relative to fred+tlrc\n"
    "                 and the output (x,y,z) are relative to fred+orig.\n"
    "                 For this to work properly, not only fred+tlrc must be\n"
    "                 readable by Vecwarp, but fred+orig must be as well.\n"
    "                 If the transformation is specified by -matvec, then\n"
    "                 the matrix-vector transformation is applied to the\n"
    "                 (x,y,z) vectors directly, with no coordinate shifting.\n"
    "\n"
    "               * AFNI .1D files with 3 columns\n"
    "                   x y z\n"
    "                   x y z\n"
    "                   et cetera...\n"
    "                 In this case, each (x,y,z) triple is transformed and\n"
    "                 written to the output.  Lines that cannot be scanned\n"
    "                 as 3 floats are treated as comments and are passed\n"
    "                 through to the output unchanged.\n"
    "               N.B.: AFNI (x,y,z) coordinates are in DICOM order:\n"
    "                   -x = Right     +x = Left\n"
    "                   -y = Anterior  +y = Posterior\n"
    "                   -z = Inferior  +z = Superior\n"
    "\n"
    " -output ooo = Write the output to file 'ooo' (to stdout if 'ooo'\n"
    "               is '-', or if the -output option is missing).  If the\n"
    "               file already exists, it will not be overwritten unless\n"
    "               the -force option is also used.\n"
    "\n"
    " -force      = If the output file already exists, -force can be\n"
    "               used to overwrite it.  If you want to use -force,\n"
    "               it must come before -output on the command line.\n"
    "\n"
    "EXAMPLES:\n"
    "\n"
    "  Vecwarp -apar fred+tlrc -input fred.orig.coord > fred.tlrc.coord\n"
    "\n"
    "This transforms the vectors defined in original coordinates to\n"
    "Talairach coordinates, using the transformation previously defined\n"
    "by AFNI markers.\n"
    "\n"
    "  Vecwarp -apar fred+tlrc -input fred.tlrc.coord -backward > fred.test.coord\n"
    "\n"
    "This does the reverse transformation; fred.test.coord should differ from\n"
    "fred.orig.coord only by roundoff error.\n"
    "\n"
    "Author: RWCox - October 2001\n"
   ) ;
   exit(0) ;
}

/*--------------------------------------------------------------------------*/

static void errex( char * str )
{
   fprintf(stderr,"** FATAL ERROR: %s\n",str) ; exit(1) ;
}

/*--------------------------------------------------------------------------*/

#define NBUF 1024
#define isnumeric(c) (isdigit(c) || c == '-' || c == '+' || c == '.')

#define SUREFIT   33
#define AFNI_1D   44

int main( int argc , char *argv[] )
{
   int iarg=1 ;
   FILE *fin=stdin , *fout=stdout ;
   int backward=0 , force=0 ;
   THD_warp *warp=NULL ;
   char lbuf[NBUF] , *cpt ;
   int itype=AFNI_1D , numv=0 , numc=0 ;
   THD_fvec3 vin , vout ;
   float xx,yy,zz ;
   int nn , ii , good=0 ;
   THD_3dim_dataset *aset=NULL , *oset=NULL ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ) Syntax() ;

   /*-- process command line arguments --*/

   while( iarg < argc ){

      /* -input */

      if( strcmp(argv[iarg],"-input") == 0 ){
         if( ++iarg >= argc )
            errex("-input: Need argument after -input") ;
         if( fin != stdin )
            errex("-input: Can't have two -input options") ;
         if( strcmp(argv[iarg],"-") != 0 ){
            fin = fopen( argv[iarg] , "r" ) ;
            if( fin == NULL )
               errex("-input: Can't open input file") ;
         }
         iarg++ ; continue ;
      }

      /* -output */

      if( strcmp(argv[iarg],"-output") == 0 ){
         if( ++iarg >= argc )
            errex("-output: Need argument after -output") ;
         if( fout != stdout )
            errex("-output: Can't have two -output options") ;
         if( strcmp(argv[iarg],"-") != 0 ){
            if( !THD_filename_ok(argv[iarg]) )
               errex("-output: Output filename is illegal") ;
            if( !force && THD_is_file(argv[iarg]) )
               errex("-output: Output file already exists") ;
            fout = fopen( argv[iarg] , "w" ) ;
            if( fout == NULL )
               errex("-output: Can't open output file") ;
         }
         iarg++ ; continue ;
      }

      /* -force */

      if( strcmp(argv[iarg],"-force") == 0 ){
         force = 1 ;
         iarg++ ; continue ;
      }

      /* -forward */

      if( strcmp(argv[iarg],"-forward") == 0 ){
         backward = 0 ;
         iarg++ ; continue ;
      }

      /* -backward */

      if( strcmp(argv[iarg],"-backward") == 0 ){
         backward = 1 ;
         iarg++ ; continue ;
      }

      /* -apar */

      if( strcmp(argv[iarg],"-apar") == 0 ){
         if( ++iarg >= argc )
            errex("-apar: Need argument after -apar") ;
         if( warp != NULL )
            errex("-apar: Can't specify transformation twice") ;

         /* open dataset with warp */

         aset = THD_open_dataset( argv[iarg] ) ;
         if( !ISVALID_DSET(aset) ){
            sprintf(lbuf,"-apar: Can't open dataset %s\n",argv[iarg]) ;
            errex(lbuf) ;
         }
         if( aset->warp == NULL ){
            sprintf(lbuf,"-apar: Dataset %s does not contain warp",argv[iarg]) ;
            errex(lbuf) ;
         }
         if( aset->view_type == VIEW_ORIGINAL_TYPE ){
            sprintf(lbuf,"-apar: Dataset %s is in the +orig view",argv[iarg]) ;
            errex(lbuf) ;
         }

         /* open +orig version of this dataset */

         sprintf(lbuf,"%s%s+orig.HEAD", aset->dblk->diskptr->directory_name,
                                        aset->dblk->diskptr->prefix         );

         oset = THD_open_dataset(lbuf) ;
         if( !ISVALID_DSET(oset) ){
            char str[NBUF] ;
            sprintf(str,"-apar: Can't open dataset %s",lbuf) ;
            errex(str) ;
         }

         warp = aset->warp ;
         iarg++ ; continue ;
      }

      /* -matvec */

      if( strcmp(argv[iarg],"-matvec") == 0 ){
         THD_dvecmat dvm ;
         THD_linear_mapping lmap ;

         if( ++iarg >= argc )
            errex("-matvec: Need argument after -matvec") ;
         if( warp != NULL )
            errex("-matvec: Can't specify transformation twice") ;
         dvm = THD_read_dvecmat( argv[iarg] , 0 ) ;
         if( SIZE_DMAT(dvm.mm) == 0.0 )
            errex("-matvec: Can't read transformation") ;

         /* manufacture an AFNI linear map */

         lmap.type = MAPPING_LINEAR_TYPE ;
         lmap.mfor.mat[0][0] = dvm.mm.mat[0][0] ;  /* copy matrix in */
         lmap.mfor.mat[0][1] = dvm.mm.mat[0][1] ;
         lmap.mfor.mat[0][2] = dvm.mm.mat[0][2] ;
         lmap.mfor.mat[1][0] = dvm.mm.mat[1][0] ;
         lmap.mfor.mat[1][1] = dvm.mm.mat[1][1] ;
         lmap.mfor.mat[1][2] = dvm.mm.mat[1][2] ;
         lmap.mfor.mat[2][0] = dvm.mm.mat[2][0] ;
         lmap.mfor.mat[2][1] = dvm.mm.mat[2][1] ;
         lmap.mfor.mat[2][2] = dvm.mm.mat[2][2] ;
         lmap.bvec.xyz[0]    = -dvm.vv.xyz[0] ;    /* copy vector in */
         lmap.bvec.xyz[1]    = -dvm.vv.xyz[1] ;
         lmap.bvec.xyz[2]    = -dvm.vv.xyz[2] ;
         LOAD_INVERSE_LMAP(lmap) ;                 /* make inverse map */

         /* manufacture an AFNI warp */

         warp = (THD_warp *) calloc( sizeof(THD_warp) , 1 ) ;
         warp->type = WARP_AFFINE_TYPE ;
         warp->rig_bod.warp = lmap ;

         fprintf(stderr,"++ Using matrix-vector transformation below:\n"
                        "   [ %9.5f %9.5f %9.5f ]   [ %9.5f ]\n"
                        "   [ %9.5f %9.5f %9.5f ]   [ %9.5f ]\n"
                        "   [ %9.5f %9.5f %9.5f ]   [ %9.5f ]\n" ,
           dvm.mm.mat[0][0] , dvm.mm.mat[0][1] , dvm.mm.mat[0][2] ,
           dvm.mm.mat[1][0] , dvm.mm.mat[1][1] , dvm.mm.mat[1][2] ,
           dvm.mm.mat[2][0] , dvm.mm.mat[2][1] , dvm.mm.mat[2][2] ,
           dvm.vv.xyz[0]    , dvm.vv.xyz[1]    , dvm.vv.xyz[2]     ) ;

         iarg++ ; continue ;
      }

      /* ERROR */

      { char *str = malloc(strlen(argv[iarg])+32) ;
        sprintf(str,"Unknown option: %s",argv[iarg]) ;
        errex(str) ;
      }

   } /* end of loop over command line args */

   /*-- check if we are ready --*/

   if( warp == NULL )
      errex("Transformation not specified") ;

   /*-- Read 1st line of input file to determine what to do with it --*/

   cpt = fgets( lbuf , NBUF , fin ) ;
   if( cpt == NULL )
      errex("Couldn't read 1st line from input file") ;

   /*-- check if this is a SureFit file --*/

   if( strstr(cpt,"BeginHeader") != NULL ) itype = SUREFIT ;
   else                                    itype = AFNI_1D ;

   /*-- if SureFit, echo all header stuff to the output --*/

   if( itype == SUREFIT ){
      int numh=1 ;
      fprintf(fout,"%s",lbuf) ;    /* echo 1st line */
      while(1){                    /* read lines until EndHeader is found */

         cpt = fgets( lbuf , NBUF , fin ) ;
         if( cpt == NULL )
            errex("Input ended before EndHeader was found") ;

         fprintf(fout,"%s",lbuf) ; /* echo line to output */
         numh++ ;

         if( strstr(lbuf,"EndHeader") != NULL ){  /* do next line, too */
            cpt = fgets( lbuf , NBUF , fin ) ;
            if( cpt == NULL )
               errex("Input ended just after EndHeader") ;
            fprintf(fout,"%s",lbuf) ;
            fprintf(stderr,"++ Wrote %d SureFit header lines to output\n",numh);
            break ;                /* end of loop */
         }
      }

      cpt = fgets( lbuf , NBUF , fin ) ;  /* get next line, with 1st vector */
      if( cpt == NULL )
         errex("Input ended just after Node count") ;

   } /* end of SureFit header echo */

   /*-- From this point on, process each line with 1 vector. --*/
   /*-- At the start of the loop, 1 line's data is in lbuf.  --*/

   do{

      switch( itype ){
         case SUREFIT:
            ii = sscanf(lbuf,"%d%f%f%f",&nn,&xx,&yy,&zz) ;
            good = (ii == 4) ;
         break ;

         case AFNI_1D:
            ii = sscanf(lbuf,"%f%f%f",&xx,&yy,&zz) ;
            good = (ii == 3) ;
         break ;
      }

      if( !good ){  /* just echo line to output */

         fprintf(fout,"%s",lbuf) ;
         numc++ ;

      } else {      /* transform a vector!!! */

         LOAD_FVEC3(vin,xx,yy,zz) ;
#ifdef DEBUG
fprintf(stderr,"\n") ;
DUMP_FVEC3("vin              ",vin) ;
#endif

         if( itype == SUREFIT && aset != NULL ){
            if( backward ) vin = THD_surefit_to_dicomm( aset , vin ) ;
            else           vin = THD_surefit_to_dicomm( oset , vin ) ;
#ifdef DEBUG
DUMP_FVEC3("surefit_to_dicomm",vin) ;
#endif
         }

         if( backward ) vout = AFNI_backward_warp_vector( warp , vin ) ;
         else           vout = AFNI_forward_warp_vector ( warp , vin ) ;

#ifdef DEBUG
DUMP_FVEC3("vout             ",vout) ;
#endif

         if( itype == SUREFIT && aset != NULL ){
            if( backward ) vout = THD_dicomm_to_surefit( oset , vout ) ;
            else           vout = THD_dicomm_to_surefit( aset , vout ) ;
#ifdef DEBUG
DUMP_FVEC3("dicomm_to_surefit",vout) ;
#endif
         }

         if( itype == SUREFIT ) fprintf(fout,"%d ",nn) ;
         fprintf(fout,"%f %f %f\n",vout.xyz[0],vout.xyz[1],vout.xyz[2]) ;
         numv++ ;
      }

      cpt = fgets( lbuf , NBUF , fin ) ;  /* get next line */

   } while( cpt != NULL ) ;  /* loop until no data can be read */

   if( numc > 0 )
      fprintf(stderr,"++ Wrote %d vectors;  %d comments\n",numv,numc) ;
   else
      fprintf(stderr,"++ Wrote %d vectors\n",numv) ;

   exit(0) ;
}

/*--------------------------------------------------------------------------
  The following functions are adapted from afni.c
----------------------------------------------------------------------------*/

/*------------------------------------------------------------------------
   Forward transform a vector following a warp
--------------------------------------------------------------------------*/

static THD_fvec3 AFNI_forward_warp_vector( THD_warp * warp , THD_fvec3 old_fv )
{
   THD_fvec3 new_fv ;

   if( warp == NULL ) return old_fv ;

   switch( warp->type ){

      default: new_fv = old_fv ; break ;

      case WARP_TALAIRACH_12_TYPE:{
         THD_linear_mapping map ;
         int iw ;

         /* forward transform each possible case,
            and test if result is in bot..top of defined map */

         for( iw=0 ; iw < 12 ; iw++ ){
            map    = warp->tal_12.warp[iw] ;
            new_fv = MATVEC_SUB(map.mfor,old_fv,map.bvec) ;

            if( new_fv.xyz[0] >= map.bot.xyz[0] &&
                new_fv.xyz[1] >= map.bot.xyz[1] &&
                new_fv.xyz[2] >= map.bot.xyz[2] &&
                new_fv.xyz[0] <= map.top.xyz[0] &&
                new_fv.xyz[1] <= map.top.xyz[1] &&
                new_fv.xyz[2] <= map.top.xyz[2]   ) break ;  /* leave loop */
         }
      }
      break ;

      case WARP_AFFINE_TYPE:{
         THD_linear_mapping map = warp->rig_bod.warp ;
         new_fv = MATVEC_SUB(map.mfor,old_fv,map.bvec) ;
      }
      break ;

   }
   return new_fv ;
}

/*------------------------------------------------------------------------
   Backward transform a vector following a warp
--------------------------------------------------------------------------*/

static THD_fvec3 AFNI_backward_warp_vector( THD_warp * warp , THD_fvec3 old_fv )
{
   THD_fvec3 new_fv ;

   if( warp == NULL ) return old_fv ;

   switch( warp->type ){

      default: new_fv = old_fv ; break ;

      case WARP_TALAIRACH_12_TYPE:{
         THD_linear_mapping map ;
         int iw ;

         /* test if input is in bot..top of each defined map */

         for( iw=0 ; iw < 12 ; iw++ ){
            map = warp->tal_12.warp[iw] ;

            if( old_fv.xyz[0] >= map.bot.xyz[0] &&
                old_fv.xyz[1] >= map.bot.xyz[1] &&
                old_fv.xyz[2] >= map.bot.xyz[2] &&
                old_fv.xyz[0] <= map.top.xyz[0] &&
                old_fv.xyz[1] <= map.top.xyz[1] &&
                old_fv.xyz[2] <= map.top.xyz[2]   ) break ;  /* leave loop */
         }
         new_fv = MATVEC_SUB(map.mbac,old_fv,map.svec) ;
      }
      break ;

      case WARP_AFFINE_TYPE:{
         THD_linear_mapping map = warp->rig_bod.warp ;
         new_fv = MATVEC_SUB(map.mbac,old_fv,map.svec) ;
      }
      break ;

   }
   return new_fv ;
}

#if 0
/*--------------------------------------------------------------------------
   The following routines are used to convert DICOM order coordinates
   (used in AFNI) to SureFit order coordinates
----------------------------------------------------------------------------*/

static THD_fvec3 THD_dicomm_to_surefit( THD_3dim_dataset *dset , THD_fvec3 fv )
{
   float xx,yy,zz , xbase,ybase,zbase ;
   THD_fvec3 vout ;
#ifdef DEBUG
static int first=1 ;
#endif

   xx = -fv.xyz[0] ; yy = -fv.xyz[1] ; zz = fv.xyz[2] ;   /* xyz now LPI */

   if( dset != NULL ){
      THD_fvec3 v1 , v2 ;
      LOAD_FVEC3(v1, DSET_XORG(dset),DSET_YORG(dset),DSET_ZORG(dset)) ;
      v1 = THD_3dmm_to_dicomm( dset , v1 ) ;
      LOAD_FVEC3(v2, DSET_XORG(dset)+(DSET_NX(dset)-1)*DSET_DX(dset) ,
                     DSET_YORG(dset)+(DSET_NY(dset)-1)*DSET_DY(dset) ,
                     DSET_ZORG(dset)+(DSET_NZ(dset)-1)*DSET_DZ(dset)  ) ;
      v2 = THD_3dmm_to_dicomm( dset , v2 ) ;
      xbase = MAX( v1.xyz[0] , v2.xyz[0] ) ; xbase = -xbase ;  /* Left-most */
      ybase = MAX( v1.xyz[1] , v2.xyz[1] ) ; ybase = -ybase ;  /* Posterior */
      zbase = MIN( v1.xyz[2] , v2.xyz[2] ) ;                   /* Inferior  */
   } else {
      xbase = ybase = zbase = 0.0 ;
   }
#ifdef DEBUG
if(first){fprintf(stderr,"d2s base=%f %f %f\n",xbase,ybase,zbase);first=0;}
#endif

   vout.xyz[0] = xx - xbase ;
   vout.xyz[1] = yy - ybase ;
   vout.xyz[2] = zz - zbase ; return vout ;
}

static THD_fvec3 THD_surefit_to_dicomm( THD_3dim_dataset *dset , THD_fvec3 fv )
{
   float xx,yy,zz , xbase,ybase,zbase ;
   THD_fvec3 vout ;
#ifdef DEBUG
static int first=1 ;
#endif

   xx = -fv.xyz[0] ; yy = -fv.xyz[1] ; zz = fv.xyz[2] ;   /* xyz now RAI */

   if( dset != NULL ){
      THD_fvec3 v1 , v2 ;
      LOAD_FVEC3(v1, DSET_XORG(dset),DSET_YORG(dset),DSET_ZORG(dset)) ;
      v1 = THD_3dmm_to_dicomm( dset , v1 ) ;
      LOAD_FVEC3(v2, DSET_XORG(dset)+(DSET_NX(dset)-1)*DSET_DX(dset) ,
                     DSET_YORG(dset)+(DSET_NY(dset)-1)*DSET_DY(dset) ,
                     DSET_ZORG(dset)+(DSET_NZ(dset)-1)*DSET_DZ(dset)  ) ;
      v2 = THD_3dmm_to_dicomm( dset , v2 ) ;
      xbase = MAX( v1.xyz[0] , v2.xyz[0] ) ; xbase = -xbase ;
      ybase = MAX( v1.xyz[1] , v2.xyz[1] ) ; ybase = -ybase ;
      zbase = MIN( v1.xyz[2] , v2.xyz[2] ) ;
   } else {
      xbase = ybase = zbase = 0.0 ;
   }
#ifdef DEBUG
if(first){fprintf(stderr,"s2d base=%f %f %f\n",xbase,ybase,zbase);first=0;}
#endif

   vout.xyz[0] = xx - xbase ;
   vout.xyz[1] = yy - ybase ;
   vout.xyz[2] = zz + zbase ; return vout ;
}
#endif
