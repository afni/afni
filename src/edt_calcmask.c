/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

/*******************************************************************
   Adapted from 3dcalc.c - RWCox - 16 Mar 2000
********************************************************************/

#include "mrilib.h"
#include "parser.h"

/*-------------------------- global data --------------------------*/

static int                CALC_nvox ;
static PARSER_code *      CALC_code ;

/*---------- dshift stuff [22 Nov 1999] ----------*/

#define DSHIFT_MODE_STOP  0
#define DSHIFT_MODE_WRAP  1
#define DSHIFT_MODE_ZERO  2

static int                CALC_dshift     [26] ; /* 22 Nov 1999 */
static int                CALC_dshift_i   [26] ;
static int                CALC_dshift_j   [26] ;
static int                CALC_dshift_k   [26] ;
static int                CALC_dshift_l   [26] ;
static int                CALC_dshift_mode[26] ;

static int                CALC_dshift_mode_current ;

/*------------------------------------------------*/

static int   CALC_has_sym[26] ;                      /* 15 Sep 1999 */
static char  abet[] = "abcdefghijklmnopqrstuvwxyz" ;

#define HAS_I  CALC_has_sym[ 8]
#define HAS_J  CALC_has_sym[ 9]
#define HAS_K  CALC_has_sym[10]
#define HAS_X  CALC_has_sym[23]
#define HAS_Y  CALC_has_sym[24]
#define HAS_Z  CALC_has_sym[25]

#define PREDEFINED_MASK ((1<< 8)|(1<< 9)|(1<<10)|(1<<23)|(1<<24)|(1<<25))

static int     CALC_has_predefined ;  /* 19 Nov 1999 */

static THD_3dim_dataset *  CALC_dset[26] ;
static int                 CALC_type[26] ;
static byte *              CALC_byte[26] ;
static short *             CALC_short[26] ;
static float *             CALC_float[26] ;
static float               CALC_ffac[26] ;

/* this macro tells if a variable (index 0..25) is defined  */

#define VAR_DEFINED(kv) (CALC_dset[kv] != NULL || CALC_dshift[kv] >= 0)

/* prototype */

static int CALC_read_opts( int argc , char * argv[] ) ;

/*------------------------------------------------------------------
  Input: cmd  = a command string, like the options for 3dcalc
         nxyz = pointer to integer

  Output: return value is a byte mask (array of 0 or 1)
          *nxyz = number of voxels in output array
  The returned array should be free()-ed when its usefulness ends.

  Example:
    byte * bm ; int ibm ;
    bm = EDT_calcmask( "-a fred+orig[7] -b ethel+orig[0]"
                       "-expr AND(step(a-99),b)" , &ibm   ) ;

    Here, bm[i] is 1 if the 7th sub-brick of fred+orig is
    greater than 99 at the i-th voxel, and at the same time
    the 0th sub-brick of ethel+orig is nonzero at the i-th voxel.
--------------------------------------------------------------------*/

byte * EDT_calcmask( char * cmd , int * nxyz )
{
   int Argc=0 ;
   char ** Argv=NULL ;
   byte * bmask ;

#define VSIZE 1024

   double * atoz[26] ;
   int ii , ids , jj, ll, jbot, jtop ;
   THD_3dim_dataset * new_dset ;
   double   temp[VSIZE];

   int   nx,nxy ;
   THD_dataxes * daxes ;

ENTRY("EDT_calcmask") ;

   /*** parse input options ***/

   if( cmd == NULL ) RETURN( NULL );
   append_string_to_args( cmd , 0,NULL , &Argc , &Argv ) ;
   if( Argc == 0 || Argv == NULL ) RETURN( NULL );

   jj = CALC_read_opts( Argc , Argv ) ;

   for( ii=0 ; ii < Argc ; ii++ ) free(Argv[ii]) ;
   free(Argv) ;

   if( jj != 0 ){
      if( CALC_code != NULL ) free(CALC_code) ;
      for( ids=0 ; ids < 26 ; ids++ ){
         if( CALC_dset[ids] != NULL ) DSET_delete( CALC_dset[ids] ) ;
      }
      RETURN( NULL );
   }

   /*** make output dataset ***/

   for( ids=0 ; ids < 26 ; ids++ ) if( CALC_dset[ids] != NULL ) break ;

   new_dset = EDIT_empty_copy( CALC_dset[ids] ) ;

   for (ids=0; ids<26; ids++)
      atoz[ids] = (double *) malloc(sizeof(double) * VSIZE ) ;

   for( ids=0 ; ids < 26 ; ids++ )  /* initialize to all zeros */
      for (ii=0; ii<VSIZE; ii++)
          atoz[ids][ii] = 0.0 ;

   nx  =      DSET_NX(new_dset) ;
   nxy = nx * DSET_NY(new_dset) ; daxes = new_dset->daxes ;

   bmask = (byte *) malloc(sizeof(byte) * CALC_nvox) ;

      /*** loop over voxels ***/

      for ( ii = 0 ; ii < CALC_nvox ; ii += VSIZE ) {

          jbot = ii ;
          jtop = MIN( ii + VSIZE , CALC_nvox ) ;

         /* loop over datasets or other symbol definitions */

          for (ids = 0 ; ids < 26 ; ids ++ ) {

            /* 22 Nov 1999: if a differentially subscripted dataset is here */

            if( CALC_dshift[ids] >= 0 ){
               int jds = CALC_dshift[ids] ;     /* actual dataset index */
               int jjs , ix,jy,kz ;
               int id=CALC_dshift_i[ids] , jd=CALC_dshift_j[ids] ,
                   kd=CALC_dshift_k[ids] , ld=CALC_dshift_l[ids] ;
               int ijkd = ((id!=0) || (jd!=0) || (kd!=0)) ;
               int dsx = DSET_NX(CALC_dset[jds]) - 1 ;
               int dsy = DSET_NY(CALC_dset[jds]) - 1 ;
               int dsz = DSET_NZ(CALC_dset[jds]) - 1 ;
               int mode = CALC_dshift_mode[ids] , dun ;

                  for( dun=0,jj=jbot ; jj < jtop ; jj++ ){
                     jjs = jj ;
                     if( ijkd ){
                        ix = DSET_index_to_ix(CALC_dset[jds],jj) ;
                        jy = DSET_index_to_jy(CALC_dset[jds],jj) ;
                        kz = DSET_index_to_kz(CALC_dset[jds],jj) ;

                        ix += id ;                  /* x shift */
                        if( ix < 0 || ix > dsx ){
                           switch( mode ){
                              case DSHIFT_MODE_ZERO:
                                 atoz[ids][jj-ii] = 0.0 ; dun = 1 ;
                              break ;
                              default:
                              case DSHIFT_MODE_STOP:
                                     if( ix <  0  ) ix = 0   ;
                                else if( ix > dsx ) ix = dsx ;
                              break ;
                              case DSHIFT_MODE_WRAP:
                                 while( ix <  0  ) ix += (dsx+1) ;
                                 while( ix > dsx ) ix -= (dsx+1) ;
                              break ;
                           }
                        }
                        if( dun ){ dun=0; continue; } /* go to next jj */

                        jy += jd ;                  /* y shift */
                        if( jy < 0 || jy > dsy ){
                           switch( mode ){
                              case DSHIFT_MODE_ZERO:
                                 atoz[ids][jj-ii] = 0.0 ; dun = 1 ;
                              break ;
                              default:
                              case DSHIFT_MODE_STOP:
                                     if( jy <  0  ) jy = 0   ;
                                else if( jy > dsy ) jy = dsy ;
                              break ;
                              case DSHIFT_MODE_WRAP:
                                 while( jy <  0  ) jy += (dsy+1) ;
                                 while( jy > dsy ) jy -= (dsy+1) ;
                              break ;
                           }
                        }
                        if( dun ){ dun=0; continue; } /* go to next jj */

                        kz += kd ;                  /* z shift */
                        if( kz < 0 || kz > dsz ){
                           switch( mode ){
                              case DSHIFT_MODE_ZERO:
                                 atoz[ids][jj-ii] = 0.0 ; dun = 1 ;
                              break ;
                              default:
                              case DSHIFT_MODE_STOP:
                                     if( kz <  0  ) kz = 0   ;
                                else if( kz > dsz ) kz = dsz ;
                              break ;
                              case DSHIFT_MODE_WRAP:
                                 while( kz <  0  ) kz += (dsz+1) ;
                                 while( kz > dsz ) kz -= (dsz+1) ;
                              break ;
                           }
                        }
                        if( dun ){ dun=0; continue; } /* go to next jj */

                        jjs = DSET_ixyz_to_index(CALC_dset[jds],ix,jy,kz) ;
                     }
                     switch( CALC_type[jds] ) {
                        case MRI_short:
                           atoz[ids][jj-ii] =  CALC_short[jds][jjs]
                                             * CALC_ffac[jds];
                        break ;
                        case MRI_float:
                           atoz[ids][jj-ii] =  CALC_float[jds][jjs]
                                             * CALC_ffac[jds];
                        break ;
                        case MRI_byte:
                           atoz[ids][jj-ii] =  CALC_byte[jds][jjs]
                                             * CALC_ffac[jds];
                        break ;
                     }
                  }
            }

            /* the case of a 3D dataset (i.e., only 1 sub-brick) */

            else if ( CALC_type[ids] >= 0 ) {
               switch( CALC_type[ids] ) {
                    case MRI_short:
                       for (jj =jbot ; jj < jtop ; jj ++ ){
                           atoz[ids][jj-ii] = CALC_short[ids][jj] * CALC_ffac[ids] ;
                     }
                    break;

                  case MRI_float:
                     for (jj =jbot ; jj < jtop ; jj ++ ){
                        atoz[ids][jj-ii] = CALC_float[ids][jj] * CALC_ffac[ids] ;
                     }
                  break;

                  case MRI_byte:
                     for (jj =jbot ; jj < jtop ; jj ++ ){
                        atoz[ids][jj-ii] = CALC_byte[ids][jj] * CALC_ffac[ids] ;
                     }
                  break;
               }
            }

           /* the case of a voxel (x,y,z) or (i,j,k) coordinate */

           else if( CALC_has_predefined ) {

              switch( ids ){
                 case 23:     /* x */
                    if( HAS_X )
                     for( jj=jbot ; jj < jtop ; jj++ )
                       atoz[ids][jj-ii] = daxes->xxorg +
                                          (jj%nx) * daxes->xxdel ;
                 break ;

                 case 24:     /* y */
                    if( HAS_Y )
                     for( jj=jbot ; jj < jtop ; jj++ )
                       atoz[ids][jj-ii] = daxes->yyorg +
                                          ((jj%nxy)/nx) * daxes->yydel ;
                 break ;

                 case 25:     /* z */
                    if( HAS_Z )
                     for( jj=jbot ; jj < jtop ; jj++ )
                       atoz[ids][jj-ii] = daxes->zzorg +
                                          (jj/nxy) * daxes->zzdel ;
                 break ;

                 case 8:     /* i */
                    if( HAS_I )
                     for( jj=jbot ; jj < jtop ; jj++ )
                       atoz[ids][jj-ii] = (jj%nx) ;
                 break ;

                 case 9:     /* j */
                    if( HAS_J )
                     for( jj=jbot ; jj < jtop ; jj++ )
                       atoz[ids][jj-ii] = ((jj%nxy)/nx) ;
                 break ;

                 case 10:    /* k */
                    if( HAS_K )
                     for( jj=jbot ; jj < jtop ; jj++ )
                       atoz[ids][jj-ii] = (jj/nxy) ;
                 break ;

               } /* end of switch on symbol subscript */

              } /* end of choice over data type (if-else cascade) */
             } /* end of loop over datasets/symbols */

            /**** actually do the work! ****/

            PARSER_evaluate_vector(CALC_code, atoz, jtop-jbot, temp);
             for ( jj = jbot ; jj < jtop ; jj ++ )
                bmask[jj] = (temp[jj-ii] != 0.0) ;

         } /* end of loop over space (voxels) */

   /* cleanup and go home */

   for( ids=0 ; ids < 26 ; ids++ ){
      free(atoz[ids]) ;
      if( CALC_dset[ids] != NULL ) DSET_delete( CALC_dset[ids] ) ;
   }
   DSET_delete(new_dset) ;
   free(CALC_code) ;

   if( nxyz != NULL ) *nxyz = CALC_nvox ;
   RETURN( bmask );
}

/*--------------------------------------------------------------------
   read the arguments, load the global variables
----------------------------------------------------------------------*/

static int CALC_read_opts( int argc , char * argv[] )
{
   int nopt = 0 ;
   int ids ;
   int ii ;

ENTRY("CALC_read_opts") ;

   CALC_nvox  = -1 ;
   CALC_code  = NULL ;
   CALC_dshift_mode_current = DSHIFT_MODE_STOP ;
   CALC_has_predefined = 0 ;

   for( ids=0 ; ids < 26 ; ids++ ){
      CALC_dset[ids]   = NULL ;
      CALC_type[ids]   = -1 ;

      CALC_dshift[ids]      = -1 ;                        /* 22 Nov 1999 */
      CALC_dshift_mode[ids] = CALC_dshift_mode_current ;
   }

   while( nopt < argc && argv[nopt][0] == '-' ){

      /**** -expr expression ****/

      if( strncmp(argv[nopt],"-expr",4) == 0 ){
         if( CALC_code != NULL ){
            fprintf(stderr,
             "** -cmask: cannot have 2 -expr options!\n") ; RETURN(1) ;
         }
         nopt++ ;
         if( nopt >= argc ){
            fprintf(stderr,
             "** -cmask: need argument after -expr!\n") ; RETURN(1) ;
         }
         CALC_code = PARSER_generate_code( argv[nopt++] ) ;
         if( CALC_code == NULL ){
            fprintf(stderr,
             "** -cmask: illegal expression!\n") ; RETURN(1) ;
         }
         PARSER_mark_symbols( CALC_code , CALC_has_sym ) ; /* 15 Sep 1999 */
         continue ;
      }

      /**** -dsSTOP [22 Nov 1999] ****/

      if( strncmp(argv[nopt],"-dsSTOP",6) == 0 ){
         CALC_dshift_mode_current = DSHIFT_MODE_STOP ;
         nopt++ ; continue ;
      }

      /**** -dsWRAP [22 Nov 1999] ****/

      if( strncmp(argv[nopt],"-dsWRAP",6) == 0 ){
         CALC_dshift_mode_current = DSHIFT_MODE_WRAP ;
         nopt++ ; continue ;
      }

      /**** -dsZERO [22 Nov 1999] ****/

      if( strncmp(argv[nopt],"-dsZERO",6) == 0 ){
         CALC_dshift_mode_current = DSHIFT_MODE_ZERO ;
         nopt++ ; continue ;
      }

      /**** -<letter> dataset ****/

      ids = strlen( argv[nopt] ) ;

      if( (argv[nopt][1] >= 'a' && argv[nopt][1] <= 'z') && ids == 2 ) {

         int ival , nxyz , ll ;
         THD_3dim_dataset * dset ;

         ival = argv[nopt][1] - 'a' ;
         if( VAR_DEFINED(ival) ){
            fprintf(stderr,
             "** -cmask: Can't define %c symbol twice\n",argv[nopt][1]);
            RETURN(1) ;
         }

         nopt++ ;
         if( nopt >= argc ){
            fprintf(stderr,
             "** -cmask: need argument after %s\n",argv[nopt-1]);
            RETURN(1) ;
         }

         /*-- 22 Nov 1999: allow for a differentially
                           subscripted name, as in "-b a[1,0,0,0]" --*/

         ll = strlen(argv[nopt]) ;
         if( (argv[nopt][0] >= 'a' && argv[nopt][0] <= 'z') &&  /* legal name */
             ( (ll >= 3 && argv[nopt][1] == '[') ||             /* subscript */
               (ll == 3 &&                                      /*    OR    */
                (argv[nopt][1] == '+' || argv[nopt][1] == '-')) /* +- ijkl */
             ) ){

            int jds = argv[nopt][0] - 'a' ;  /* actual dataset index */
            int * ijkl ;                     /* array of subscripts */

            if( CALC_dset[jds] == NULL ){
               fprintf(stderr,
                "** -cmask: Must define dataset %c before using it in %s\n",
                       argv[nopt][0] , argv[nopt] ) ;
               RETURN(1) ;
            }

            /*- get subscripts -*/

            if( argv[nopt][1] == '[' ){            /* format is [i,j,k,l] */
               MCW_intlist_allow_negative(1) ;
               ijkl = MCW_get_intlist( 9999 , argv[nopt]+1 ) ;
               MCW_intlist_allow_negative(0) ;
               if( ijkl == NULL || ijkl[0] != 4 ){
                  fprintf(stderr,
                   "** -cmask: Illegal differential subscripting %s\n",
                                 argv[nopt] ) ;
                  RETURN(1) ;
               }
            } else {                               /* format is +i, -j, etc */
                ijkl = (int *) malloc( sizeof(int) * 5 ) ;
                ijkl[1] = ijkl[2] = ijkl[3] = ijkl[4] = 0 ;  /* initialize */
                switch( argv[nopt][2] ){
                   default:
                      fprintf(stderr,
                       "** -cmask: Bad differential subscripting %s\n",
                                 argv[nopt] ) ;
                   RETURN(1) ;

                   case 'i': ijkl[1] = (argv[nopt][1]=='+') ? 1 : -1 ; break ;
                   case 'j': ijkl[2] = (argv[nopt][1]=='+') ? 1 : -1 ; break ;
                   case 'k': ijkl[3] = (argv[nopt][1]=='+') ? 1 : -1 ; break ;
                   case 'l': ijkl[4] = (argv[nopt][1]=='+') ? 1 : -1 ; break ;
                }
            }

            if( ijkl[4] != 0 ){  /* disallow time subscripting */
               fprintf(stderr,
                "++ -cmask: Warning: differential time shifting %s not allowed\n",
                       argv[nopt] ) ;
               ijkl[4] = 0 ;
            }

            /*- more sanity checks -*/

            if( ijkl[1]==0 && ijkl[2]==0 && ijkl[3]==0 && ijkl[4]==0 ){
               fprintf(stderr,
                "++ -cmask: differential subscript %s is all zero\n",
                       argv[nopt] ) ;
            }

            /*- set values for later use -*/

            CALC_dshift  [ival] = jds ;
            CALC_dshift_i[ival] = ijkl[1] ;
            CALC_dshift_j[ival] = ijkl[2] ;
            CALC_dshift_k[ival] = ijkl[3] ;
            CALC_dshift_l[ival] = ijkl[4] ;

            CALC_dshift_mode[ival] = CALC_dshift_mode_current ;

            /*- time to trot, Bwana -*/

            free(ijkl) ; nopt++ ; goto DSET_DONE ;

         } /* end of _dshift */

         /*-- meanwhile, back at the "normal" dataset opening ranch --*/

         { char dname[512] ;                               /* 02 Nov 1999 */

           if( strstr(argv[nopt],"[") == NULL ){
              sprintf(dname,"%s[0]",argv[nopt++]) ;        /* add [0] */
           } else {
              strcpy(dname,argv[nopt++]) ;                 /* don't mangle */
           }
           dset = THD_open_dataset( dname ) ;              /* open it */
           if( dset == NULL ){
              fprintf(stderr,
               "** -cmask: can't open dataset %s\n",dname) ;
              RETURN(1) ;
           }
         }
         CALC_dset[ival] = dset ;

         /* set some parameters based on the dataset */

         nxyz = dset->daxes->nxx * dset->daxes->nyy * dset->daxes->nzz ;
         if( CALC_nvox < 0 ){
            CALC_nvox = nxyz ;
         } else if( nxyz != CALC_nvox ){
            fprintf(stderr,
             "** -cmask: dataset %s differs in size from others\n",argv[nopt-1]);
            RETURN(1) ;
         }

         CALC_type[ival] = DSET_BRICK_TYPE(dset,0) ;

         /* load floating scale factors */

         CALC_ffac[ival] = DSET_BRICK_FACTOR(dset,0) ;
         if (CALC_ffac[ival] == 0.0 ) CALC_ffac[ival] = 1.0 ;

         /* read data from disk */

         THD_load_datablock( dset->dblk ) ;
         if( ! DSET_LOADED(dset) ){
            fprintf(stderr,
             "** -cmask: Can't read data brick for dataset %s\n",argv[nopt-1]) ;
            RETURN(1) ;
         }

         /* set pointers for actual dataset arrays */

         switch (CALC_type[ival]) {
             case MRI_short:
                CALC_short[ival] = (short *) DSET_ARRAY(dset,0) ;
             break;

             case MRI_float:
                CALC_float[ival] = (float *) DSET_ARRAY(dset,0) ;
             break;

             case MRI_byte:
                CALC_byte[ival] = (byte *) DSET_ARRAY(dset,0) ;
             break;

          } /* end of switch over type */

DSET_DONE: continue;

      } /* end of dataset input */

      fprintf(stderr,"** -cmask: Unknown option: %s\n",argv[nopt]) ;
      RETURN(1) ;

   }  /* end of loop over options */

   /*---------------------------------------*/
   /*** cleanup: check for various errors ***/

   for( ids=0 ; ids < 26 ; ids++ ) if( CALC_dset[ids] != NULL ) break ;
   if( ids == 26 ){
      fprintf(stderr,
       "** -cmask: No actual input datasets given!\n") ;
      RETURN(1) ;
   }

   if( CALC_code == NULL ){
      fprintf(stderr,"** -cmask: No expression given!\n") ;
      RETURN(1) ;
   }

   /* 15 Apr 1999: check if each input dataset is used,
                   or if an undefined symbol is used.   */

   for (ids=0; ids < 26; ids ++){
      if( VAR_DEFINED(ids) && !CALC_has_sym[ids] )
         fprintf(stderr ,
          "++ -cmask: input '%c' is not used in the expression\n" ,
              abet[ids] ) ;

      else if( !VAR_DEFINED(ids) && CALC_has_sym[ids] ){

         if( ((1<<ids) & PREDEFINED_MASK) == 0 )
            fprintf(stderr ,
             "++ -cmask: symbol %c is used but not defined\n" ,
                    abet[ids] ) ;
         else {
            CALC_has_predefined++ ;
         }
      }
   }

   RETURN(0) ;
}
