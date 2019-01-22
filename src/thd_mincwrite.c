#include "mrilib.h"
#include <signal.h>

#ifndef DONT_ALLOW_MINC

/*******************************************************************/
/*!  Write an AFNI dataset as a MINC file.
     - fname = MINC filename
     - dset = AFNI dataset
     - flags = logical OR of various masks:
     - MINC_FLOATIZE_MASK => force output in float format
     - MINC_SWAPIZE_MASK => add -swap_bytes option  
   Return value is 1 if went OK, 0 if not.
-------------------------------------------------------------------*/

int THD_write_minc( char *fname, THD_3dim_dataset *dset, int flags )
{
   static char *pg=NULL ; static int first=1 ;

   int nvals,nvox , iv , floatize , ii , datum , good=1 ;
   THD_dataxes *dax ;
   char  axcode[3] ;
   float axstep[3] , axstart[3] ;
   int   axnum[3] ;
   char *cmd ;
   FILE *fp ;
   void *bar ;
   float *far=NULL ;

ENTRY("THD_write_minc") ;

   /*-- check inputs for goodness --*/

   if( !THD_filename_ok(fname) || fname[0] == '-' ){
      fprintf(stderr,"** ERROR: Illegal filename for MINC output: %s\n",
              (fname != NULL) ? fname : "(null)" ) ;
      RETURN(0) ;
   }

   if( !ISVALID_DSET(dset) ){
      fprintf(stderr,
              "** ERROR: Illegal input dataset for MINC output: %s\n",
              fname ) ;
      RETURN(0) ;
   }

   /*-- find rawtominc program to do the real work --*/

   if( first ){
     pg = THD_find_executable("rawtominc") ; first = 0 ;
   }

   if( pg == NULL ){
     fprintf(stderr,
             "** ERROR: Can't write MINC file without program rawtominc: %s\n",
             fname);
     RETURN(0) ;
   }

   /*-- load dataset from disk, if need be --*/

   DSET_load(dset) ;
   if( !DSET_LOADED(dset) ){
     fprintf(stderr,
             "** ERROR: Can't write MINC file since dataset isn't loaded: %s\n",
             fname) ;
     RETURN(0) ;
   }

   /*-- check sub-bricks for uniformity in type, legal type, etc. --*/

   nvals    = DSET_NVALS(dset) ;
   nvox     = DSET_NVOX(dset) ;
   floatize = (flags & MINC_FLOATIZE_MASK) != 0 ;
   ii       = DSET_BRICK_TYPE(dset,0) ;

   for( iv=0 ; iv < nvals ; iv++ ){
     if( DSET_BRICK_TYPE(dset,iv) == MRI_complex ){
       fprintf(stderr,
               "** ERROR: Can't write MINC file since dataset is complex: %s\n",
               fname) ;
       RETURN(0) ;
     }
     if( DSET_BRICK_TYPE(dset,iv) == MRI_rgb ){
       fprintf(stderr,
               "** ERROR: Can't write MINC file since dataset is RGB: %s\n",
               fname) ;
       RETURN(0) ;
     }

     /* check if must convert to floats */

     if( !floatize ){
        if( DSET_BRICK_TYPE(dset,iv)   == MRI_float ||
            DSET_BRICK_FACTOR(dset,iv) != 0.0       ||
            DSET_BRICK_TYPE(dset,iv)   != ii          ){

          floatize = 1 ;
          fprintf(stderr,
                  "++ NOTE: Writing MINC file in float format: %s\n",
                  fname) ;
        }
     }
   }

   if( floatize ) datum = MRI_float ;
   else           datum = ii ;

   /*-- load geometry information --*/

   dax = dset->daxes ;

   axcode[0] = ORIENT_xyz[ dax->xxorient ] ; axnum[0] = dax->nxx ;
   axcode[1] = ORIENT_xyz[ dax->yyorient ] ; axnum[1] = dax->nyy ;
   axcode[2] = ORIENT_xyz[ dax->zzorient ] ; axnum[2] = dax->nzz ;

   axstep[0] = dax->xxdel ; axstart[0] = dax->xxorg ;
   axstep[1] = dax->yydel ; axstart[1] = dax->yyorg ;
   axstep[2] = dax->zzdel ; axstart[2] = dax->zzorg ;

   if( axcode[0] == 'x' || axcode[0] == 'y' ){
     axstep[0] = -axstep[0] ; axstart[0] = -axstart[0] ;
   }

   if( axcode[1] == 'x' || axcode[1] == 'y' ){
     axstep[1] = -axstep[1] ; axstart[1] = -axstart[1] ;
   }

   if( axcode[2] == 'x' || axcode[2] == 'y' ){
     axstep[2] = -axstep[2] ; axstart[2] = -axstart[2] ;
   }

   /*-- start to create command --*/

   cmd = AFMALL(char, 65500) ; /* long enough?  */
   strcpy(cmd,pg) ;      /* basic command */

   /* -- swap action */
   if (flags & MINC_SWAPIZE_MASK) {
      sprintf(cmd+strlen(cmd)," -swap_bytes");
   }
   
   /* axes orientation */
   
   sprintf(cmd+strlen(cmd)," -%c%c%c",axcode[2],axcode[1],axcode[0]) ;

   /* input and output data type */

   sprintf(cmd+strlen(cmd)," -%s -o%s",
           MRI_TYPE_name[datum],MRI_TYPE_name[datum]) ;

   /* axis stuff */

   sprintf(cmd+strlen(cmd),
           " -%cstep %.3f -%cstep %.3f -%cstep %.3f"
           " -%cstart %.3f -%cstart %.3f -%cstart %.3f"
           " -xdircos 1 0 0 -ydircos 0 1 0 -zdircos 0 0 1" ,
           axcode[0],axstep[0] ,axcode[1],axstep[1] ,axcode[2],axstep[2]  ,
           axcode[0],axstart[0],axcode[1],axstart[1],axcode[2],axstart[2]  ) ;

   /*-- do we create a time step attribute? --*/

   if( nvals > 1 & DSET_NUM_TIMES(dset) > 1 && DSET_TIMESTEP(dset) > 0.0 ){
     float tr = DSET_TIMESTEP(dset) ;
     sprintf(cmd+strlen(cmd)," -dattribute time:step=%.3f",tr) ;
     sprintf(cmd+strlen(cmd)," -frame_times '") ;
     for( iv=0 ; iv < nvals ; iv++ )
       sprintf(cmd+strlen(cmd),"%.3f ",iv*tr) ;
     strcat(cmd,"'") ;
   }

   /*-- Is this Talaraich? --*/

   if( dset->view_type == VIEW_TALAIRACH_TYPE ){
     sprintf(cmd+strlen(cmd)," -sattribute xspace:spacetype=talairach_") ;
     sprintf(cmd+strlen(cmd)," -sattribute yspace:spacetype=talairach_") ;
     sprintf(cmd+strlen(cmd)," -sattribute zspace:spacetype=talairach_") ;
   }

   /*-- integer datasets need to be scanned    --*/
   /*-- and have valid_range set (stupid MINC) --*/

   if( !floatize ){
     sprintf(cmd+strlen(cmd)," -scan_range") ;
     switch( datum ){
        case MRI_short:
          sprintf(cmd+strlen(cmd)," -range 0 32767") ; break ;

        case MRI_byte:
          sprintf(cmd+strlen(cmd)," -range 0 255")   ; break ;
     }
   }

   /*-- add output file name --*/

   sprintf(cmd+strlen(cmd)," %s",fname) ;

   /*-- add # sub-bricks, if > 1 --*/

   if( nvals > 1 ) sprintf(cmd+strlen(cmd)," %d",nvals) ;

   /*-- add number of points along each axis --*/

   sprintf(cmd+strlen(cmd)," %d %d %d" , axnum[2],axnum[1],axnum[0] ) ;

   /*-- execute the command as a filter --*/
   fprintf(stderr,"About to run %s\n", cmd);
   signal( SIGPIPE , SIG_IGN ) ; errno = 0 ;
   fp = popen( cmd , "w" ) ;
   if( fp == NULL ){
     fprintf(stderr,"** ERROR: Can't open MINC output filter: %s\a\n",cmd) ;
     if( errno != 0 ) perror("** Unix error message") ;
     free(cmd) ; RETURN(0) ;
   }

   /*-- allocate space for floatizing --*/

   if( floatize ){
     far = (float *) malloc(sizeof(float)*nvox) ;
     if( far == NULL ){
       fprintf(stderr,
               "** ERROR: Can't write MINC file due to lack of memory: %s\n",
               fname) ;
       free(cmd) ; RETURN(0) ;
     }
   }

   /*-- loop through sub-bricks, convert to floats if needed, write to pipe --*/

   for( iv=0 ; iv < nvals ; iv++ ){
      bar = DSET_ARRAY(dset,iv) ; errno = 0 ;
      if( floatize ){
        EDIT_coerce_scale_type( nvox ,
                                DSET_BRICK_FACTOR(dset,iv) ,
                                DSET_BRICK_TYPE(dset,iv)   , bar ,
                                MRI_float                  , far  ) ;
        ii = fwrite( far , sizeof(float)         , nvox , fp ) ;
      } else {
        ii = fwrite( bar , mri_datum_size(datum) , nvox , fp ) ;
      }

      DSET_unload_one(dset,iv) ;

      if( ii < nvox ){
        fprintf(stderr,
                "** ERROR: fwrite to MINC failed at iv=%d: %s\n",
                iv , fname ) ;
        if( errno ) perror("fwrite") ;
        if( ii == 0 ){ good=0; break; }       /* don't try to continue */
      }
   }

   if( floatize ) free(far) ;  /* no longer needed */

   /*-- close pipe --*/

   ii = pclose(fp) ;
   if( ii == -1 ){
     perror("** ERROR in MINC write filter") ;
     fprintf(stderr,"** MINC filter command was %s\n\a",cmd) ;
     free(cmd) ; RETURN(0) ;
   }

   /*-- and we are done --*/

   free(cmd) ; RETURN(good) ;
}

#else

int THD_write_minc( char *fname, THD_3dim_dataset *dset, int flags )
{
   ERROR_message("MINC-1 dataset output disabled") ; return 0 ;
}

#endif
