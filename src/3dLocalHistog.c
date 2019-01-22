#include "mrilib.h"

#ifdef USE_OMP
#include <omp.h>
#endif

#undef  UINT32
#define UINT32 unsigned int

#undef  TWO15
#undef  T15MM
#undef  TWO16
#define TWO15 32768
#define T15MM 32767
#define TWO16 65536

#define ALLOW_PROB

THD_3dim_dataset * THD_localhistog( int , THD_3dim_dataset ** ,
                                    int , int * , MCW_cluster * , int,int ) ;

/*----------------------------------------------------------------------------*/

void usage_3dLocalHistog(int detail)
{
        printf(
"Usage: 3dLocalHistog [options] dataset ... \n"
"\n"
"This program computes, at each voxel, a count of how many times each\n"
"unique value occurs in a neighbhood of that voxel, across all the input\n"
"datasets.\n"
" * The neighborhood is defined by the '-nbhd' option.\n"
" * The input datasets should be in short or byte format, without\n"
"   scaling factors attached.\n"
" * You can input float format datasets, but the values will be rounded\n"
"   to an integer between -32767 and 32767 before being used.\n"
" * You can also output the overall histogram of the dataset collection,\n"
"   via the '-hsave' option (as a 1D file).  This is simply the count of how\n"
"   many times each value occurs.\n"
" * For histograms of continuously valued datasets see program 3dLocalstat \n"
"   with option -stat hist* \n"
"\n"
"OPTIONS\n"
"-------\n"
" -nbhd 'nnn'   = The string 'nnn' defines the region around each\n"
"                 voxel that will be extracted for the statistics\n"
"                 calculation.  The format of the 'nnn' string is\n"
"                 the same as in 3dLocalstat:\n"
"                 * 'SPHERE(r)'\n"
"                 * 'RECT(a,b,c)'\n"
"                 * 'RHDD(a)'\n"
"                 * 'TOHD(a)'\n"
"                 * If no '-nbhd' option is given, then just the voxel\n"
"                   itself is used -- in which case, the input dataset(s)\n"
"                   must comprise a total of at least 2 sub-bricks!\n"
"\n"
" -prefix ppp   = Use string 'ppp' as the prefix for the output dataset.\n"
"\n"
" -hsave sss    = Save the overall histogram into file 'sss'.  This file will\n"
"                 have 2 columns:   value  count\n"
"                 Values with zero count will not be shown in this file.\n"
"\n"
" -lab_file LL  = Use file 'LL' as a label file.  The first column contains\n"
"                 the numbers, the second column the corresponding labels.\n"
"                 * You can use a column selector to choose the columns you\n"
"                   want.  For example, if the first column has the labels\n"
"                   and the second the values, use 'filename[1,0]'.\n"
"\n"
" -exclude a..b = Exclude values from 'a' to 'b' from the counting.\n"
"                 * Zero (0) will never be excluded.\n"
"                 * You can use '-exclude' more than once.\n"
" -excNONLAB    = If '-lab_file' is used, then exclude all values that are NOT\n"
"                 in the label file (except for 0, of course).\n"
" -mincount mm  = Exclude values which appear in the overall histogram\n"
"                 fewer than 'mm' times.\n"
"                 * Excluded  values will be treated as if they are zero\n"
"                   (and so appear in the '0:Other' output sub-brick).\n"
"                 * The overall histogram output by '-hsave' is NOT altered\n"
"                   by the use of '-mincount' or '-exclude' or '-excNONLAB'.\n"
#ifdef ALLOW_PROB
"\n"
" -prob         = Normally, the output dataset is a set of counts.  This\n"
"                 option converts each count to a 'probability' by dividing\n"
"                 by the total number of counts at each voxel.\n"
"                 * The resulting dataset is stored as bytes, in units of\n"
"                   0.01, so that p=1 corresponds to 1/0.01=100.\n"
#endif
"\n"
" -quiet        = Stop the highly informative progress reports.\n"
"\n"
"OUTPUT DATASET\n"
"--------------\n"
" * For each distinct value a sub-brick is produced.\n"
" * The zero value will be first; after that, the values will appear in\n"
"   increasing order.\n"
" * If '-lab_file' is used, then the sub-brick label for a given value's count\n"
"   will be of the form 'value:label'; for example, '2013:rh.lingual'.\n"
" * For values NOT in the '-lab_file', the label will just be of the form 'value:'.\n"
" * For the first (value=0) sub-brick, the label will be '0:Other'.\n"
"\n"
"Author: RWCox - April 2013\n"
     ) ;
     PRINT_COMPILE_DATE ;
   return;
}
/*-----------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *inset=NULL , *outset ;
   THD_3dim_dataset **insar=NULL ; int nsar=0 ;
   int iarg=1 , ii,kk , ids ;
   MCW_cluster *nbhd=NULL ;
   char *prefix="./localhistog" ;
   int ntype=0 ; float na=0.0f,nb=0.0f,nc=0.0f ;
   int verb=1 , do_prob=0 ;
   int nx=0,ny=0,nz=0,nvox=0, rbot,rtop ;
   char *labfile=NULL ; NI_element *labnel=NULL ;
   int nlab=0 , *labval=NULL ; char **lablab=NULL ; char buf[THD_MAX_SBLABEL] ;
   UINT32 *ohist , *mhist=NULL ; char *ohist_name=NULL ; int ohzadd=0 ;
   int *rlist , numval ; float mincount=0.0f ; int mcc ;
   int *exlist=NULL, numex=0 ;
   int do_excNONLAB=0 ;

   /*---- for the clueless who wish to become clued-in ----*/

   if( argc == 1 ){ usage_3dLocalHistog(1); exit(0); } /* Bob's help shortcut */

   /*---- official startup ---*/

#if defined(USING_MCW_MALLOC) && !defined(USE_OMP)
   enable_mcw_malloc() ;
#endif

   PRINT_VERSION("3dLocalHistog"); mainENTRY("3dLocalHistog main"); machdep();
   AFNI_logger("3dLocalHistog",argc,argv);
   if( getpid()%2 ) AUTHOR("Bilbo Baggins"); else AUTHOR("Thorin Oakenshield");
   AFNI_SETUP_OMP(0) ;  /* 24 Jun 2013 */

   /*---- loop over options ----*/

   while( iarg < argc && argv[iarg][0] == '-' ){

     if( strcmp(argv[iarg],"-help") == 0 || strcmp(argv[iarg],"-h") == 0){
        usage_3dLocalHistog(strlen(argv[iarg])>3 ? 2:1);
        exit(0);
     }

     if( strncmp(argv[iarg],"-qu",3) == 0 ){
       verb = 0 ; iarg++ ; continue ;
     }
     if( strncmp(argv[iarg],"-verb",5) == 0 ){
       verb++ ; iarg++ ; continue ;
     }

#ifdef ALLOW_PROB
     if( strncmp(argv[iarg],"-prob",5) == 0 ){
       do_prob = 1 ; iarg++ ; continue ;
     }
#endif

     if( strcmp(argv[iarg],"-exclude") == 0 ){
       int ebot=-6666666,etop=-6666666 , ee ;
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-exclude'") ;
       sscanf(argv[iarg],"%d..%d",&ebot,&etop) ;
       if( ebot >= -TWO15 && ebot <= TWO15 ){
         if( etop < -TWO15 || etop > TWO15 || etop < ebot ) etop = ebot ;
         exlist = (int *)realloc(exlist,sizeof(int)*(etop-ebot+1+numex+1)) ;
         for( ee=ebot ; ee <= etop ; ee++ ){ if( ee != 0 ) exlist[numex++] = ee ; }
       }
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-excNONLAB") == 0 ){
       do_excNONLAB = 1 ; iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-prefix") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-prefix'") ;
       prefix = strdup(argv[iarg]) ;
       if( !THD_filename_ok(prefix) ) ERROR_exit("Bad -prefix!") ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-hsave") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-hsave'") ;
       ohist_name = strdup(argv[iarg]) ;
       if( !THD_filename_ok(ohist_name) ) ERROR_exit("Bad -hsave!") ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-mincount") == 0 ){
       char *cpt ;
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-mincount'") ;
       mincount = (float)strtod(argv[iarg],&cpt) ;
#if 0
       if( mincount > 0.0f && mincount < 50.0f && *cpt == '%' )  /* percentage */
         mincount = -0.01f*mincount ;
#endif
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-nbhd") == 0 ){
       char *cpt ;
       if( ntype  >  0    ) ERROR_exit("Can't have 2 '-nbhd' options") ;
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-nbhd'") ;

       cpt = argv[iarg] ;
       if( strncasecmp(cpt,"SPHERE",6) == 0 ){
         sscanf( cpt+7 , "%f" , &na ) ;
         ntype = NTYPE_SPHERE ;
       } else if( strncasecmp(cpt,"RECT",4) == 0 ){
         sscanf( cpt+5 , "%f,%f,%f" , &na,&nb,&nc ) ;
         if( na == 0.0f && nb == 0.0f && nc == 0.0f )
           ERROR_exit("'RECT(0,0,0)' is not a legal neighborhood") ;
         ntype = NTYPE_RECT ;
       } else if( strncasecmp(cpt,"RHDD",4) == 0 ){
         sscanf( cpt+5 , "%f" , &na ) ;
         if( na == 0.0f ) ERROR_exit("Can't have a RHDD of radius 0") ;
         ntype = NTYPE_RHDD ;
       } else if( strncasecmp(cpt,"TOHD",4) == 0 ){
         sscanf( cpt+5 , "%f" , &na ) ;
         if( na == 0.0f ) ERROR_exit("Can't have a TOHD of radius 0") ;
         ntype = NTYPE_TOHD ;
       } else {
         ERROR_exit("Unknown -nbhd shape: '%s'",cpt) ;
       }
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-lab_file") == 0 || strcmp(argv[iarg],"-labfile") == 0 ){
       char **labnum ; int nbad=0 ;
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]) ;
       if( labfile != NULL ) ERROR_exit("Can't use '%s' twice!",argv[iarg-1]) ;
       labfile = strdup(argv[iarg]) ;
       labnel = THD_string_table_read(labfile,0) ;
       if( labnel == NULL || labnel->vec_num < 2 )
         ERROR_exit("Can't read label file '%s'",labfile) ;
       nlab   = labnel->vec_len ;
       labnum = (char **)labnel->vec[0] ;
       lablab = (char **)labnel->vec[1] ;
       labval = (int *)calloc(sizeof(int),nlab) ;
       for( ii=0 ; ii < nlab ; ii++ ){
         if( labnum[ii] != NULL ){
           labval[ii] = (int)strtod(labnum[ii],NULL) ;
           if( labval[ii] < -TWO15 || labval[ii] > TWO15 ){ labval[ii] = 0; nbad++; }
         }
       }
       if( nbad > 0 )
         ERROR_message("%d label values are outside the range %d..%d :-(" ,
         nbad , -TWO15 , TWO15 ) ;
       iarg++ ; continue ;
     }

     ERROR_message("** 3dLocalHistog: Illegal option: '%s'",argv[iarg]) ;
     suggest_best_prog_option(argv[0], argv[iarg]);
     exit(1) ;

   } /*--- end of loop over options ---*/

   /*---- check for stupid user inputs ----*/

   if( iarg >= argc ) ERROR_exit("No datasets on command line?") ;

   if( ohist_name == NULL && strcmp(prefix,"NULL") == 0 )
     ERROR_exit("-prefix NULL is only meaningful if you also use -hsave :-(") ;

   /*------------ scan input datasets, built overall histogram ------------*/

   nsar  = argc - iarg ;
   insar = (THD_3dim_dataset **)malloc(sizeof(THD_3dim_dataset *)*nsar) ;

   if( verb ) fprintf(stderr,"Scanning %d datasets ",nsar) ;

   ohist = (UINT32 *)calloc(sizeof(UINT32),TWO16) ;

   for( ids=iarg ; ids < argc ; ids++ ){                      /* dataset loop */
     insar[ids-iarg] = inset = THD_open_dataset(argv[ids]) ;
     CHECK_OPEN_ERROR(inset,argv[ids]) ;
     if( ids == iarg ){
       nx = DSET_NX(inset); ny = DSET_NY(inset); nz = DSET_NZ(inset); nvox = nx*ny*nz;
     } else if( nx != DSET_NX(inset) ||
                ny != DSET_NY(inset) || nz != DSET_NZ(inset) ){
       ERROR_exit("Dataset %s grid doesn't match!",argv[ids]) ;
     }
     if( !THD_datum_constant(inset->dblk) )
       ERROR_exit("Dataset %s doesn't have a fixed data type! :-(",argv[ids]) ;
     if( THD_need_brick_factor(inset) )
       ERROR_exit("Dataset %s has scale factors! :-(",argv[ids]) ;
     if( DSET_BRICK_TYPE(inset,0) != MRI_byte  &&
         DSET_BRICK_TYPE(inset,0) != MRI_short &&
         DSET_BRICK_TYPE(inset,0) != MRI_float    )
       ERROR_exit("Dataset %s is not byte- or short-valued! :-(",argv[ids]) ;
     DSET_load(inset) ; CHECK_LOAD_ERROR(inset) ;

     for( ii=0 ; ii < DSET_NVALS(inset) ; ii++ ){ /* add to overall histogram */
       if( verb ) fprintf(stderr,".") ;
       switch( DSET_BRICK_TYPE(inset,ii) ){
         case MRI_short:{
           short *sar = (short *)DSET_BRICK_ARRAY(inset,ii) ;
           for( kk=0 ; kk < nvox ; kk++ ) ohist[ sar[kk]+TWO15 ]++ ;
         }
         break ;
         case MRI_byte:{
           byte *bar = (byte *)DSET_BRICK_ARRAY(inset,ii) ;
           for( kk=0 ; kk < nvox ; kk++ ) ohist[ bar[kk]+TWO15 ]++ ;
         }
         break ;
         case MRI_float:{
           float *far = (float *)DSET_BRICK_ARRAY(inset,ii) ; short ss ;
           for( kk=0 ; kk < nvox ; kk++ ){ ss = SHORTIZE(far[kk]); ohist[ss+TWO15]++; }
         }
         break ;
       }
     } /* end of sub-brick loop */

     DSET_unload(inset) ;  /* will re-load later, as needed */

   } /* end of dataset loop */

   if( verb ) fprintf(stderr,"\n") ;

   /*-------------- process overall histogram for fun and profit -------------*/

   /* if we didn't actually find 0, put it in the histogram now */

   if( ohist[0+TWO15] == 0 ){ ohist[0+TWO15] = 1 ; ohzadd = 1 ; }

   /* excNONLAB? */

   if( nlab > 0 && do_excNONLAB ){
     byte *klist = (byte *)calloc(sizeof(byte),TWO16) ; int nee ;
     for(     ii=0 ; ii < nlab  ; ii++ ){ if( labval[ii] != 0 ) klist[labval[ii]+TWO15] = 1 ; }
     for( nee=ii=0 ; ii < TWO16 ; ii++ ){ if( !klist[ii] ) nee++ ; }
     exlist = (int *)realloc(exlist,sizeof(int)*(numex+nee+1)) ;
     for(     ii=0 ; ii < TWO16 ; ii++ ){ if( ii != TWO15 && !klist[ii] ) exlist[numex++] = ii-TWO15 ; }
     free(klist) ;
   }

   /* make a copy of ohist and edit it for mincount, etc */

   mhist = (UINT32 *)malloc(sizeof(UINT32)*TWO16) ;
   memcpy(mhist,ohist,sizeof(UINT32)*TWO16) ;
   mcc = (mincount < 0.0f) ? (int)(-mincount*nvox) : (int)mincount ;
   if( mcc > 1 ){
     for( ids=ii=0 ; ii < TWO16 ; ii++ ){
       if( ii != TWO15 && mhist[ii] > 0 && mhist[ii] < mcc ){ mhist[ii] = 0; ids++; }
     }
     if( ids > 0 && verb )
       INFO_message("Edited out %d values with overall histogram counts less than %d",ids,mcc) ;
   }
   if( numex > 0 ){
     int ee ;
     for( ids=0,ii=0 ; ii < numex ; ii++ ){
       ee = exlist[ii] ;
       if( mhist[ee+TWO15] > 0 ){ mhist[ee+TWO15] = 0; ids++; }
     }
     free(exlist) ;
     if( ids > 0 && verb )
       INFO_message("Edited out %d values from the exclude list",ids) ;
   }

   /* count number of values with nonzero (edited) counts */

   numval = 0 ;
   for( ii=0 ; ii < TWO16 ; ii++ ) if( mhist[ii] != 0 ) numval++ ;

   if( numval == 0 ) ERROR_exit("Nothing found! WTF?") ;  /* should not happen */

   /* make list of all values with nonzero (edited) count */

   rlist = (int *)malloc(sizeof(int)*numval) ;
   if( verb > 1 ) fprintf(stderr,"++ Include list:") ;
   for( ii=kk=0 ; ii < TWO16 ; ii++ ){
     if( mhist[ii] != 0 ){
       rlist[kk++] = ii-TWO15 ;
       if( verb > 1 ) fprintf(stderr," %d[%u]",ii-TWO15,mhist[ii]) ;
     }
   }
   if( verb > 1 ) fprintf(stderr,"\n") ;

   rbot = rlist[0] ; rtop = rlist[numval-1] ; /* smallest and largest values found */

   if( rbot == rtop ) ERROR_exit("Only one value (%d) found in all inputs!",rbot) ;

   /* if 0 isn't first in rlist, then
      put it in first place and move negative values up by one spot */

   if( rbot < 0 ){
     for( kk=0 ; kk < numval && rlist[kk] != 0 ; kk++ ) ; /*nada*/
     if( kk < numval ){   /* should always be true */
       for( ii=kk-1 ; ii >= 0 ; ii-- ) rlist[ii+1] = rlist[ii] ;
       rlist[0] = 0 ;
     }
   }

   if( verb )
     INFO_message("Value range = %d..%d (%d distinct values)",rbot,rtop,numval );

   /* save overall histogram? */

   if( ohist_name != NULL ){
     FILE *fp = fopen(ohist_name,"w") ; int nl=0 ;
     if( fp == NULL ) ERROR_exit("Can't open -hsave '%s' for output!",ohist_name) ;
     if( ohzadd ) ohist[0+TWO15] = 0 ;
     for( ii=0 ; ii < TWO16 ; ii++ ){
       if( ohist[ii] != 0 ){ fprintf(fp,"%6d %u\n",ii-TWO15,ohist[ii]); nl++; }
     }
     fclose(fp) ;
     if( verb ) INFO_message("Wrote %d lines to -hsave file %s",nl,ohist_name) ;
   }

   free(ohist) ; free(mhist) ; mhist = ohist = NULL ;  /* done with this */

   if( strcmp(prefix,"NULL") == 0 ) exit(0) ;   /* special case */

   /*----------- build the neighborhood mask -----------*/

   if( ntype <= 0 ){         /* default neighborhood */
     ntype = NTYPE_SPHERE ; na = 0.0f ;
     if( verb ) INFO_message("Using default neighborhood = self") ;
   }

   switch( ntype ){
     default:
       ERROR_exit("WTF?  ntype=%d",ntype) ;  /* should not happen */

     case NTYPE_SPHERE:{
       float dx , dy , dz ;
       if( na < 0.0f ){ dx = dy = dz = 1.0f ; na = -na ; }
       else           { dx = fabsf(DSET_DX(insar[0])) ;
                        dy = fabsf(DSET_DY(insar[0])) ;
                        dz = fabsf(DSET_DZ(insar[0])) ; }
       nbhd = MCW_spheremask( dx,dy,dz , na ) ;
     }
     break ;

     case NTYPE_RECT:{
       float dx , dy , dz ;
       if( na < 0.0f ){ dx = 1.0f; na = -na; } else dx = fabsf(DSET_DX(insar[0]));
       if( nb < 0.0f ){ dy = 1.0f; nb = -nb; } else dy = fabsf(DSET_DY(insar[0]));
       if( nc < 0.0f ){ dz = 1.0f; nc = -nc; } else dz = fabsf(DSET_DZ(insar[0]));
       nbhd = MCW_rectmask( dx,dy,dz , na,nb,nc ) ;
     }
     break ;

     case NTYPE_RHDD:{
       float dx , dy , dz ;
       if( na < 0.0f ){ dx = dy = dz = 1.0f ; na = -na ; }
       else           { dx = fabsf(DSET_DX(insar[0])) ;
                        dy = fabsf(DSET_DY(insar[0])) ;
                        dz = fabsf(DSET_DZ(insar[0])) ; }
       nbhd = MCW_rhddmask( dx,dy,dz , na ) ;
     }
     break ;

     case NTYPE_TOHD:{
       float dx , dy , dz ;
       if( na < 0.0f ){ dx = dy = dz = 1.0f ; na = -na ; }
       else           { dx = fabsf(DSET_DX(insar[0])) ;
                        dy = fabsf(DSET_DY(insar[0])) ;
                        dz = fabsf(DSET_DZ(insar[0])) ; }
       nbhd = MCW_tohdmask( dx,dy,dz , na ) ;
     }
     break ;
   }

   if( verb ) INFO_message("Neighborhood comprises %d voxels",nbhd->num_pt) ;

   /*------- actually do some work for a change (is it lunchtime yet?) -------*/

   if( verb ) fprintf(stderr,"Voxel-wise histograms ") ;

   outset = THD_localhistog( nsar,insar , numval,rlist , nbhd , do_prob,verb ) ;

   if( outset == NULL ) ERROR_exit("Function THD_localhistog() fails?!") ;

   /*---- save resulting dataset ----*/

   EDIT_dset_items( outset , ADN_prefix,prefix , ADN_none ) ;

   tross_Copy_History( insar[0] , outset ) ;
   tross_Make_History( "3dLocalHistog" , argc,argv , outset ) ;

   /* but first attach labels to sub-bricks */

   EDIT_BRICK_LABEL(outset,0,"0:Other") ;
   for( kk=1 ; kk < numval ; kk++ ){
     sprintf(buf,"%d:",rlist[kk]) ;
     for( ii=0 ; ii < nlab ; ii++ ){
       if( labval[ii] == rlist[kk] && lablab[ii] != NULL ){
         ids = strlen(buf) ;
         MCW_strncpy(buf+ids,lablab[ii],THD_MAX_SBLABEL-ids) ;
         break ;
       }
     }
     EDIT_BRICK_LABEL(outset,kk,buf) ;
   }

   DSET_write( outset ) ;
   if( verb ) WROTE_DSET( outset ) ;
   exit(0) ;
}

/*----------------------------------------------------------------------------*/

THD_3dim_dataset * THD_localhistog( int nsar , THD_3dim_dataset **insar ,
                                    int numval , int *rlist , MCW_cluster *nbhd ,
                                    int do_prob , int verb )
{
   THD_3dim_dataset *outset=NULL , *inset ;
   int nvox=DSET_NVOX(insar[0]) ;
   int ids, iv, bb, nnpt=nbhd->num_pt ;
   MRI_IMAGE *bbim ; int btyp ;
   float **outar , **listar ;

ENTRY("THD_localhistog") ;

   /*---- create output dataset ----*/

   outset = EDIT_empty_copy(insar[0]) ;
   EDIT_dset_items( outset ,
                      ADN_nvals     , numval    ,
                      ADN_datum_all , MRI_float ,
                      ADN_nsl       , 0         ,
                      ADN_brick_fac , NULL      ,
                    ADN_none ) ;
   outar = (float **)malloc(sizeof(float *)*numval) ;
   for( bb=0 ; bb < numval ; bb++ ){
     EDIT_substitute_brick( outset , bb , MRI_float , NULL ) ;
     outar[bb] = DSET_BRICK_ARRAY(outset,bb) ;
   }

   /*---- make mapping between values and arrays to get those values ----*/

   listar = (float **)malloc(sizeof(float *)*TWO16) ;
   for( bb=0 ; bb < TWO16 ; bb++ ) listar[bb] = outar[0] ;
   for( bb=1 ; bb < numval ; bb++ ){
     listar[ rlist[bb] + TWO15 ] = outar[bb] ;
   }

   /*----------- loop over datasets, add in counts for all voxels -----------*/

   for( ids=0 ; ids < nsar ; ids++ ){              /* dataset loop */
     inset = insar[ids] ; DSET_load(inset) ;
     for( iv=0 ; iv < DSET_NVALS(inset) ; iv++ ){  /* sub-brick loop */
       if( verb ) fprintf(stderr,".") ;
       bbim = DSET_BRICK(inset,iv) ; btyp = bbim->kind ;
       if( nnpt == 1 ){                            /* only 1 voxel in nbhd */
         int qq,ii,jj,kk,ib,nb ;
         switch( bbim->kind ){
           case MRI_short:{
             short *sar = MRI_SHORT_PTR(bbim) ;
             for( qq=0 ; qq < nvox ; qq++ ) listar[sar[qq]+TWO15][qq]++ ;
           }
           break ;
           case MRI_byte:{
             byte *bar = MRI_BYTE_PTR(bbim) ;
             for( qq=0 ; qq < nvox ; qq++ ) listar[bar[qq]+TWO15][qq]++ ;
           }
           break ;
           case MRI_float:{
             float *far = MRI_FLOAT_PTR(bbim) ; short ss ;
             for( qq=0 ; qq < nvox ; qq++ ){ ss = SHORTIZE(far[qq]); listar[ss+TWO15][qq]++; }
           }
           break ;
         }
       } else {                                    /* multiple voxels in nbhd */
 AFNI_OMP_START ;
#pragma omp parallel
 { int qq,ii,jj,kk,ib,nb ; void *nar ; short *sar,ss ; byte *bar ; float *far ;
   nar = malloc(sizeof(float)*nnpt) ;
   sar = (short *)nar ; bar = (byte *)nar ; far = (float *)nar ;
#pragma omp for
         for( qq=0 ; qq < nvox ; qq++ ){           /* qq=voxel index */
           ii = DSET_index_to_ix(inset,qq) ;
           jj = DSET_index_to_jy(inset,qq) ;
           kk = DSET_index_to_kz(inset,qq) ;
           nb = mri_get_nbhd_array( bbim , NULL , ii,jj,kk , nbhd , nar ) ;
           if( nb == 0 ) continue ;
           switch( btyp ){
             case MRI_short:
               for( ib=0 ; ib < nb ; ib++ ) listar[sar[ib]+TWO15][qq]++ ;
             break ;
             case MRI_byte:
               for( ib=0 ; ib < nb ; ib++ ) listar[bar[ib]+TWO15][qq]++ ;
             break ;
             case MRI_float:
               for( ib=0 ; ib < nb ; ib++ ){ ss = SHORTIZE(far[ib]); listar[ss+TWO15][qq]++; }
             break ;
           }
         } /* end of voxel loop */
   free(nar) ;
 } /* end of OpenMP */
 AFNI_OMP_END ;
       }
     } /* end of sub-brick loop */
     DSET_unload(inset) ;
   } /* end of dataset loop */

   if( verb ) fprintf(stderr,"\n") ;

   free(listar) ;

   /*---- post-process output ---*/

   if( do_prob ){
     byte **bbar ; int pp ;
 
     if( verb ) INFO_message("Conversion to probabilities") ;

 AFNI_OMP_START ;
#pragma omp parallel
 { int qq , ib ; float pfac , val ; byte **bbar ;
#pragma omp for
     for( qq=0 ; qq < nvox ; qq++ ){
       pfac = 0.0001f ;
       for( ib=0 ; ib < numval ; ib++ ) pfac += outar[ib][qq] ;
       pfac = 250.0f / pfac ;
       for( ib=0 ; ib < numval ; ib++ ){
         val = outar[ib][qq]*pfac ; outar[ib][qq] = BYTEIZE(val) ;
       }
     }
 } /* end OpenMP */
 AFNI_OMP_END ;

     bbar = (byte **)malloc(sizeof(byte *)*numval) ;
     for( bb=0 ; bb < numval ; bb++ ){
       bbar[bb] = (byte *)malloc(sizeof(byte)*nvox) ;
       for( pp=0 ; pp < nvox ; pp++ ) bbar[bb][pp] = (byte)outar[bb][pp] ;
       EDIT_substitute_brick(outset,bb,MRI_byte,bbar[bb]) ;
       EDIT_BRICK_FACTOR(outset,bb,0.004f) ;
     }
     free(bbar) ;

   } /* end of do_prob */

   free(outar) ;
   RETURN(outset) ;
}
