/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"
#include "thd.h"

/*--------------------------------------------------------------------*/
/*! Inline version of 3dinfo.
    - You must free() the output string when done with it.
    - verbose is -1 (shortest output), 0, or 1 (longest output)
----------------------------------------------------------------------*/

#include <stdarg.h>

#undef  ZMAX              /* these values must be the same as in debugtrace.c */
#undef  SZMAX
#define ZMAX  32222       /* increased for Ziad (who else is so crazy?) */
#define SZMAX "%.32222s"  /* same as ZMAX */

/* There is also: storage_mode_name() */
const char * storage_mode_str(int mode) {
   switch(mode) {
      default:
         return("Undefined");
      case STORAGE_BY_BRICK:
         return("BRIK") ;
      case STORAGE_BY_MINC:
         return("MINC") ; 
      case STORAGE_BY_VOLUMES:
         return("Volume") ; 
      case STORAGE_BY_ANALYZE:
         return("ANALYZE") ; 
      case STORAGE_BY_CTFMRI:
         return("CTF MRI") ; 
      case STORAGE_BY_CTFSAM:
         return("CTF SAM") ; 
      case STORAGE_BY_1D:
         return("AFNI .1D") ; 
      case STORAGE_BY_3D:
         return("AFNI .3D") ; 
      case STORAGE_BY_NIFTI:
         return("NIFTI") ; 
      case STORAGE_BY_MPEG:
         return("MPEG") ; 
      case STORAGE_BY_NIML:   
         return("NIML") ;
      case STORAGE_BY_NI_SURF_DSET:
         return("NI_SURF_DSET") ;
      case STORAGE_BY_NI_TRACT:
         return("NI_TRACT") ;
      case STORAGE_BY_GIFTI:
         return("GIFTI") ;
    }
}

/* Return the prefix of a dataset without file extensions 
   Returned string must be freed */
char *DSET_prefix_noext(THD_3dim_dataset *dset) 
{ 
   char *ppp, *eee, *ccc=NULL;
   int ii;
   
   if (!dset) return(NULL);
   
   ppp = DSET_PREFIX(dset);
   if (!ppp) ppp = "NO_PREFIX";
   
   ccc = (char *)malloc(sizeof(char)*(1+strlen(ppp)));
   ccc[0]='\0';
   eee = find_filename_extension( ppp );
   if (!eee) {
      strcpy(ccc, ppp);
   } else {
      ii = 0;
      while (ppp < eee) {
         ccc[ii] = *ppp; ppp++; ++ii;
      }
      ccc[ii]='\0';
   }
   return(ccc);
}

int dset_obliquity(THD_3dim_dataset *dset , float *anglep)
{
   int obliquity = -1;
   float angle= 0.0f;
      
   if(ISVALID_MAT44(dset->daxes->ijk_to_dicom_real)) {
      angle = THD_compute_oblique_angle(dset->daxes->ijk_to_dicom_real, 0);
      if(angle>0.0) {
         obliquity = 1;
      } else {
         obliquity = 0;
      }
   }
   if (anglep) *anglep = angle;
   return(obliquity);
}

/* common tolerance is OBLIQ_ANGLE_THRESH = 0.01 */
double dset_obliquity_angle_diff(THD_3dim_dataset *dset1, 
                                 THD_3dim_dataset *dset2, 
                                 double tol) 
{
   if (!dset1 || !dset1->daxes ||
       !dset2 || !dset2->daxes ) return(0.0);
   THD_check_oblique_field(dset1);
   THD_check_oblique_field(dset2);
   return(daxes_obliquity_angle_diff(dset1->daxes, dset2->daxes,tol));
}

/* common tolerance is OBLIQ_ANGLE_THRESH = 0.01 */
double daxes_obliquity_angle_diff(THD_dataxes *ax1, THD_dataxes *ax2, 
                                  double tol) 
{
   double angle, rangle;
   if (!ax1 || !ax2) return(0.0);
   angle = THD_compute_oblique_angle(ax1->ijk_to_dicom_real, 0);
   rangle = THD_compute_oblique_angle(ax2->ijk_to_dicom_real, 0);
   rangle = angle-rangle; 
   if (rangle < 0.0) rangle = -rangle;
   if (rangle < tol) rangle = 0.0;
   return(rangle);
}

/* 
   A debugging function to show the state of the various file naming fields
*/
void THD_show_dataset_names( THD_3dim_dataset *dset, char *head, FILE *out) 
{
   if (!dset) {
      fprintf(stderr,"NULL dset"); 
      return;
   }
   if (!out) out = stderr;
   
   if (head && !strcmp(head,"FOR_3DINFO")) {
      fprintf(out, "    filecode: %s"
                      "    header_name: %s"
                      "    brick_name: %s"
                      "    prefix: %s"
                      "    storage_mode: %d",
            dset->dblk->diskptr->filecode,
            dset->dblk->diskptr->header_name,
            dset->dblk->diskptr->brick_name,
            DSET_PREFIX(dset),
            dset->dblk->diskptr->storage_mode);
   } else {
      fprintf(out, "*** FileLove: %s\n"
                      "    filecode: %s\n"
                      "    header_name: %s\n"
                      "    brick_name: %s\n"
                      "    prefix: %s\n"
                      "    storage_mode: %d\n",
            head ? head:"",
            dset->dblk->diskptr->filecode,
            dset->dblk->diskptr->header_name,
            dset->dblk->diskptr->brick_name,
            DSET_PREFIX(dset),
            dset->dblk->diskptr->storage_mode);
   }
   return;
}

int THD_dset_minmax (THD_3dim_dataset *dset, int scl, 
                         float *min, float *max)
{
   int i=0;
   float mm, MM;
   
   *min = 0.0; *max = 0.0;
   if (!dset) return(0);
   
   for (i=0; i<DSET_NVALS(dset); ++i) {
      if (!THD_subbrick_minmax (dset, i, scl, &mm, &MM)) {
         return(0);
      } else {
         if (i==0) {
            *min = mm; *max = MM;
         } else {
            if (mm < *min) *min = mm;
            if (MM > *max) *max = MM;
         }
      }
   }
   return(1);
}

float THD_dset_max(THD_3dim_dataset *dset, int scl) {
   float max,min;
   if (!THD_dset_minmax(dset, scl,&min, &max)) {
      ERROR_message("Could not get dset min max");
   }
   return(max);
}

float THD_dset_min(THD_3dim_dataset *dset, int scl) {
   float max,min;
   if (THD_dset_minmax(dset, scl,&min, &max)) {
      ERROR_message("Could not get dset min max");
   }
   return(min);
}

int THD_subbrick_minmax (THD_3dim_dataset *dset, int isb, int scl, 
                         float *min, float *max)
{
   float tf = 1.0;
   
   *min = 0.0; *max = 0.0;
   if (!dset) return(0);
   RELOAD_STATS(dset);  
    
   if( ISVALID_STATISTIC(dset->stats) ) {
      *min = dset->stats->bstat[isb].min;
      *max = dset->stats->bstat[isb].max;
   } else { /* the slow way */
      THD_slow_minmax_dset(dset, min, max, isb, isb);
   }

   /* values are already scaled, remove it */
   if (!scl) {
      tf = DSET_BRICK_FACTOR(dset,isb) ; if (tf == 0.0) tf = 1.0;
      *min /= tf;
      *max /= tf;
   }

   return(1);
}

/* get min and max of data in range of sub-bricks of dataset */
/* - not using STATS, so 3dSkullStrip can use NIfTI datasets */
/* - renamed from minmax_dset to THD_slow_minmax_dset        */
/* - moved from 3dhistog.c               18 Dec 2012 [rickr] */
int THD_slow_minmax_dset(THD_3dim_dataset *dset, float *dmin, float *dmax,
                         int iv_bot, int iv_top)
{
   int iv_fim;
   float fimfac;
   float vbot , vtop, temp_fbot=1.0, temp_ftop=0.0 ;

   DSET_load(dset);

   for( iv_fim=iv_bot ; iv_fim <= iv_top ; iv_fim++ ){
     /* minimum and maximum for sub-brick */
     vbot = mri_min( DSET_BRICK(dset,iv_fim) ) ;
     vtop = mri_max( DSET_BRICK(dset,iv_fim) ) ;
     fimfac = DSET_BRICK_FACTOR(dset,iv_fim) ;
     if (fimfac == 0.0)  fimfac = 1.0;
     vbot *= fimfac ; vtop *= fimfac ;

     /* update global min and max */
     if ( temp_fbot > temp_ftop ) { /* first time, just copy */
        temp_fbot = vbot;
        temp_ftop = vtop;
     } else {
        if( vbot < temp_fbot ) temp_fbot = vbot;
        if( vtop > temp_ftop ) temp_ftop = vtop;
     }
   }
   *dmin = temp_fbot;
   *dmax = temp_ftop;

   return(0);
}


float THD_subbrick_max(THD_3dim_dataset *dset, int isb, int scl) {
   float max,min;
   if (!THD_subbrick_minmax(dset, isb, scl,&min, &max)) {
      ERROR_message("Could not get min max");
   }
   return(max);
}

float THD_subbrick_min(THD_3dim_dataset *dset, int isb, int scl) {
   float max,min;
   if (THD_subbrick_minmax(dset, isb, scl,&min, &max)) {
      ERROR_message("Could not get min max");
   }
   return(min);
}

float THD_dset_extent(THD_3dim_dataset *dset, char ret, float *RL_AP_IS)
{
   THD_dataxes      *daxes ;
   THD_fvec3 fv1 , fv2  ;
   float tf;
   char *xlbot , *xltop , *ylbot , *yltop , *zlbot , *zltop ;
   static char *RR="[R]" , *LL="[L]" ,
               *PP="[P]" , *AA="[A]" ,
               *SS="[S]" , *II="[I]" , *ZZ="   " ;

   ENTRY("THD_dset_extent") ;

   if( ! ISVALID_3DIM_DATASET(dset) ) RETURN(0.0) ;

   daxes = dset->daxes ;
   LOAD_FVEC3(fv1 , daxes->xxorg , daxes->yyorg , daxes->zzorg) ;
   fv1 = THD_3dmm_to_dicomm( dset , fv1 ) ;

   LOAD_FVEC3(fv2 , daxes->xxorg + (daxes->nxx-1)*daxes->xxdel ,
                    daxes->yyorg + (daxes->nyy-1)*daxes->yydel ,
                    daxes->zzorg + (daxes->nzz-1)*daxes->zzdel  ) ;
   fv2 = THD_3dmm_to_dicomm( dset , fv2 ) ;

   if( fv1.xyz[0] > fv2.xyz[0] ) FSWAP( fv1.xyz[0] , fv2.xyz[0] ) ;
   if( fv1.xyz[1] > fv2.xyz[1] ) FSWAP( fv1.xyz[1] , fv2.xyz[1] ) ;
   if( fv1.xyz[2] > fv2.xyz[2] ) FSWAP( fv1.xyz[2] , fv2.xyz[2] ) ;

   XLAB(xlbot,fv1.xyz[0]) ; YLAB(ylbot,fv1.xyz[1]) ; ZLAB(zlbot,fv1.xyz[2]) ;
   XLAB(xltop,fv2.xyz[0]) ; YLAB(yltop,fv2.xyz[1]) ; ZLAB(zltop,fv2.xyz[2]) ;

   if (RL_AP_IS) {
    RL_AP_IS[0] = fv1.xyz[0];
    RL_AP_IS[1] = fv2.xyz[0];
    RL_AP_IS[2] = fv1.xyz[1];
    RL_AP_IS[3] = fv2.xyz[1];
    RL_AP_IS[4] = fv1.xyz[2];
    RL_AP_IS[5] = fv2.xyz[2];
   }

   switch (ret) {
    case 'R':
      return(fv1.xyz[0]);
    case 'L':
      return(fv2.xyz[0]);
    case 'A':
      return(fv1.xyz[1]);
    case 'P':
      return(fv2.xyz[1]);
    case 'I':
      return(fv1.xyz[2]);
    case 'S':
      return(fv2.xyz[2]);
    case '-':
    default:
      if (!RL_AP_IS) {
        ERROR_message("Nothing being returned");
      }
      return(0.0);
   }

}

char * THD_dataset_info( THD_3dim_dataset *dset , int verbose )
{
   THD_dataxes      *daxes ;
   THD_fvec3 fv1 , fv2 , fv3 ;
   int ival , ntimes , nval_per , n1,n2,n3 , kv,npar ;
   float tf, angle=0.0;
   long long tb ;

   static char *RR="[R]" , *LL="[L]" ,
               *PP="[P]" , *AA="[A]" ,
               *SS="[S]" , *II="[I]" , *ZZ="   " ;
   char *xlbot , *xltop , *ylbot , *yltop , *zlbot , *zltop , *cpt ;
   char str[1024], soblq[1024] ;
   int nstr , obliquity;

   char *outbuf = NULL ;  /* output buffer */

ENTRY("THD_dataset_info") ;

   if( ! ISVALID_3DIM_DATASET(dset) ) RETURN(NULL) ;

   daxes = dset->daxes ;

   if( DSET_IS_BRIK(dset) )
     outbuf = THD_zzprintf(outbuf,"Dataset File:    %s\n" , DSET_FILECODE(dset) ) ;
   else
     outbuf = THD_zzprintf(outbuf,"Dataset File:    %s\n" , DSET_BRIKNAME(dset) ) ;

   outbuf = THD_zzprintf(outbuf,"Identifier Code: %s  Creation Date: %s\n" ,
             dset->idcode.str , dset->idcode.date ) ;
   outbuf = THD_zzprintf(outbuf,   "Template Space:  %s\n", dset->atlas_space);

   if( ISANAT(dset) ){
      outbuf = THD_zzprintf(outbuf,"Dataset Type:    %s (-%s)\n",
                ANAT_typestr[dset->func_type] , ANAT_prefixstr[dset->func_type] ) ;
   } else {
      outbuf = THD_zzprintf(outbuf,"Dataset Type:    %s (-%s)\n",
                FUNC_typestr[dset->func_type] , FUNC_prefixstr[dset->func_type] ) ;
   }

   /* 25 April 1998: do byte order stuff */

   switch( DSET_BYTEORDER(dset) ){
      case LSB_FIRST:
         outbuf = THD_zzprintf(outbuf,"Byte Order:      %s" , LSB_FIRST_STRING) ;
      break ;
      case MSB_FIRST:
         outbuf = THD_zzprintf(outbuf,"Byte Order:      %s" , MSB_FIRST_STRING) ;
      break ;
   }

   if( THD_find_string_atr(dset->dblk,ATRNAME_BYTEORDER) == NULL ) /* 19 Sep 1999 */
      outbuf = THD_zzprintf(outbuf," {assumed}") ;

   kv = mri_short_order() ;
   switch( kv ){
      case LSB_FIRST:
         outbuf = THD_zzprintf(outbuf," [this CPU native = %s]\n" , LSB_FIRST_STRING) ;
      break ;
      case MSB_FIRST:
         outbuf = THD_zzprintf(outbuf," [this CPU native = %s]\n" , MSB_FIRST_STRING) ;
      break ;
   }

   /*-- 21 Jun 2002: print storage mode --*/
   if( dset->dblk->diskptr != NULL ){
      outbuf = THD_zzprintf(outbuf,"Storage Mode:    %s\n",
                        storage_mode_str(dset->dblk->diskptr->storage_mode));
   }

   tb = dset->dblk->total_bytes ;
   if( tb > 0 )
     outbuf = THD_zzprintf(outbuf,"Storage Space:   %s (%s) bytes\n",
                           commaized_integer_string(dset->dblk->total_bytes) ,
                           approximate_number_string(dset->dblk->total_bytes) ) ;

   /*-- keywords --*/

   if( verbose >= 0 ){
     cpt = DSET_KEYWORDS(dset) ;
     if( cpt != NULL && cpt[0] != '\0' ){
       int j = strlen(cpt) ;
       if( j < 99 ){
         outbuf = THD_zzprintf(outbuf,"Keywords:        %s\n" , cpt ) ;
       } else {
        int k ;
        outbuf = THD_zzprintf(outbuf,"\n----- KEYWORDS -----\n") ;
        for( k=0 ; k < j ; k += ZMAX )
          outbuf = THD_zzprintf(outbuf,SZMAX,cpt+k) ;
         outbuf = THD_zzprintf(outbuf,"\n") ;
       }
     }
   }

   /*-- idcodes --*/

  if( verbose >= 0 ){
   if( ! ISZERO_IDCODE(dset->anat_parent_idcode) )
      outbuf = THD_zzprintf(outbuf,"Anatomy Parent:  %s [%s]\n" ,
                dset->anat_parent_name , dset->anat_parent_idcode.str ) ;
   else if( strlen(dset->anat_parent_name) > 0 )
      outbuf = THD_zzprintf(outbuf,"Anatomy Parent:  %s\n" , dset->anat_parent_name ) ;

   if( ! ISZERO_IDCODE(dset->warp_parent_idcode) )
      outbuf = THD_zzprintf(outbuf,"Warp Parent:     %s [%s]\n" ,
                 dset->warp_parent_name , dset->warp_parent_idcode.str) ;
   else if( strlen(dset->warp_parent_name) > 0 )
      outbuf = THD_zzprintf(outbuf,"Warp Parent:     %s\n" , dset->warp_parent_name ) ;
  }

   /*-- tagset --*/
   if( verbose > 0 && dset->tagset != NULL && dset->tagset->num > 0 ){
      int ii , ns=0 ;
      for( ii=0 ; ii < dset->tagset->num ; ii++ )
         if( dset->tagset->tag[ii].set ) ns++ ;

      outbuf = THD_zzprintf(outbuf,"Tagset:          %d set [out of %d total]\n",
                            ns , dset->tagset->num ) ;
   }

   /* are we oblique ? */
   if((obliquity = dset_obliquity(dset, &angle)) >= 0) {
      if(angle>0.0) {
         sprintf (soblq,
            "Data Axes Tilt:  Oblique (%.3f deg. from plumb)\n"
            "Data Axes Approximate Orientation:",
            angle);
      } else {
         sprintf (soblq,
            "Data Axes Tilt:  Plumb\n"
            "Data Axes Orientation:");
      }
      { char *gstr = EDIT_get_geometry_string(dset) ;
        if( gstr != NULL && *gstr != '\0' )
          outbuf = THD_zzprintf(outbuf,"Geometry String: \"%s\"\n",gstr) ;
      }
   } else {
      sprintf (soblq,
            "Data Axes Tilt:  Unspecified, assumed plumb\n"
            "Data Axes Orientation:");
   }

   outbuf = THD_zzprintf(outbuf,
      "%s\n"
      "  first  (x) = %s\n"
      "  second (y) = %s\n"
      "  third  (z) = %s   [-orient %c%c%c]\n" ,
    soblq,
    ORIENT_typestr[daxes->xxorient] ,
      ORIENT_typestr[daxes->yyorient] ,
      ORIENT_typestr[daxes->zzorient] ,
    ORIENT_typestr[daxes->xxorient][0] ,
      ORIENT_typestr[daxes->yyorient][0] ,
      ORIENT_typestr[daxes->zzorient][0]  ) ;

   LOAD_FVEC3(fv1 , daxes->xxorg , daxes->yyorg , daxes->zzorg) ;
   fv1 = THD_3dmm_to_dicomm( dset , fv1 ) ;

   LOAD_FVEC3(fv2 , daxes->xxorg + (daxes->nxx-1)*daxes->xxdel ,
                    daxes->yyorg + (daxes->nyy-1)*daxes->yydel ,
                    daxes->zzorg + (daxes->nzz-1)*daxes->zzdel  ) ;
   fv2 = THD_3dmm_to_dicomm( dset , fv2 ) ;

   if( fv1.xyz[0] > fv2.xyz[0] ) FSWAP( fv1.xyz[0] , fv2.xyz[0] ) ;
   if( fv1.xyz[1] > fv2.xyz[1] ) FSWAP( fv1.xyz[1] , fv2.xyz[1] ) ;
   if( fv1.xyz[2] > fv2.xyz[2] ) FSWAP( fv1.xyz[2] , fv2.xyz[2] ) ;

   LOAD_FVEC3(fv3 , daxes->xxdel , daxes->yydel , daxes->zzdel) ;
   fv3 = THD_3dmm_to_dicomm( dset , fv3 ) ;

   XLAB(xlbot,fv1.xyz[0]) ; YLAB(ylbot,fv1.xyz[1]) ; ZLAB(zlbot,fv1.xyz[2]) ;
   XLAB(xltop,fv2.xyz[0]) ; YLAB(yltop,fv2.xyz[1]) ; ZLAB(zltop,fv2.xyz[2]) ;

   n1 = DAXES_NUM(daxes,ORI_R2L_TYPE) ;
   n2 = DAXES_NUM(daxes,ORI_A2P_TYPE) ;
   n3 = DAXES_NUM(daxes,ORI_I2S_TYPE) ;

   outbuf = THD_zzprintf(outbuf,
      "R-to-L extent: %9.3f %s -to- %9.3f %s -step- %9.3f mm [%3d voxels]\n"
      "A-to-P extent: %9.3f %s -to- %9.3f %s -step- %9.3f mm [%3d voxels]\n"
      "I-to-S extent: %9.3f %s -to- %9.3f %s -step- %9.3f mm [%3d voxels]\n" ,
    fv1.xyz[0],xlbot , fv2.xyz[0],xltop , fabs(fv3.xyz[0]) , n1 ,
    fv1.xyz[1],ylbot , fv2.xyz[1],yltop , fabs(fv3.xyz[1]) , n2 ,
    fv1.xyz[2],zlbot , fv2.xyz[2],zltop , fabs(fv3.xyz[2]) , n3  ) ;

   /*-- 01 Feb 2001: print the center of the dataset as well --*/

   if( verbose > 0 ){
    fv1.xyz[0] = 0.5*(fv1.xyz[0]+fv2.xyz[0]) ; XLAB(xlbot,fv1.xyz[0]) ;
    fv1.xyz[1] = 0.5*(fv1.xyz[1]+fv2.xyz[1]) ; YLAB(ylbot,fv1.xyz[1]) ;
    fv1.xyz[2] = 0.5*(fv1.xyz[2]+fv2.xyz[2]) ; ZLAB(zlbot,fv1.xyz[2]) ;

    outbuf = THD_zzprintf(outbuf,
                            "R-to-L center: %9.3f %s\n"
                            "A-to-P center: %9.3f %s\n"
                            "I-to-S center: %9.3f %s\n" ,
                          fv1.xyz[0],xlbot ,
                          fv1.xyz[1],ylbot ,
                          fv1.xyz[2],zlbot  ) ;
   }

   ntimes   = DSET_NUM_TIMES(dset) ;
   nval_per = DSET_NVALS_PER_TIME(dset) ;
   if( ntimes > 1 ){

      outbuf = THD_zzprintf(outbuf,
         "Number of time steps = %d" , ntimes ) ;

      STATUS("timestep") ;

      outbuf = THD_zzprintf(outbuf, "  Time step = %.5f%s  Origin = %.5f%s" ,
                 dset->taxis->ttdel ,
                 UNITS_TYPE_LABEL(dset->taxis->units_type) ,
                 dset->taxis->ttorg ,
                 UNITS_TYPE_LABEL(dset->taxis->units_type)  ) ;
      if( dset->taxis->nsl > 0 )
        outbuf = THD_zzprintf(outbuf,"  Number time-offset slices = %d  Thickness = %.3f",
                  dset->taxis->nsl , fabs(dset->taxis->dz_sl) ) ;
      outbuf = THD_zzprintf(outbuf,"\n") ;

      STATUS("nsl done") ;

      if( verbose > 0 && dset->taxis->nsl > 0 ){
         outbuf = THD_zzprintf(outbuf,"Time-offsets per slice:") ;
         if( dset->taxis->toff_sl != NULL ){
           for( ival=0 ; ival < dset->taxis->nsl ; ival++ )
             outbuf = THD_zzprintf(outbuf, " %.3f" , dset->taxis->toff_sl[ival] ) ;
           outbuf = THD_zzprintf(outbuf,"\n") ;
         } else {
             outbuf = THD_zzprintf(outbuf,"NOT AVAILABLE\n") ;
         }
      }
   } else {
      outbuf = THD_zzprintf(outbuf,
           "Number of values stored at each pixel = %d\n" , nval_per ) ;
   }

#if 0
   if( verbose > 0 && ntimes > 1 ) nval_per = dset->dblk->nvals ;
   else                            nval_per = 1 ;                 /* 12 Feb 2002 */
#else
   nval_per = dset->dblk->nvals ;
   if( verbose < 0 && nval_per > 5 ) nval_per = 3 ;
#endif

   /* print out stuff for each sub-brick */

   for( ival=0 ; ival < nval_per ; ival++ ){

     STATUS("ival a") ;

      sprintf( str ,
               "  -- At sub-brick #%d '%s' datum type is %s" ,
               ival , DSET_BRICK_LAB(dset,ival) ,
               MRI_TYPE_name[DSET_BRICK_TYPE(dset,ival)] ) ;
      nstr = strlen(str) ;

      tf = DSET_BRICK_FACTOR(dset,ival) ;

      if( ISVALID_STATISTIC(dset->stats) ){

         if( tf != 0.0 ){
            sprintf( str+nstr ,
                                ":%13.6g to %13.6g [internal]\n"
                    "%*s[*%13.6g] %13.6g to %13.6g [scaled]\n" ,
                    dset->stats->bstat[ival].min/tf ,
                    dset->stats->bstat[ival].max/tf ,
                    nstr-16," " , tf ,
                    dset->stats->bstat[ival].min , dset->stats->bstat[ival].max ) ;
          } else {
            sprintf( str+nstr , ":%13.6g to %13.6g\n" ,
                    dset->stats->bstat[ival].min , dset->stats->bstat[ival].max ) ;
          }
      } else if( tf != 0.0 ){
         sprintf( str+nstr , " [*%g]\n",tf) ;
      } else {
         sprintf( str+nstr , "\n") ;
      }
     STATUS("ival b") ;
      outbuf = THD_zzprintf(outbuf,"%s",str) ;

      /** 30 Nov 1997: print sub-brick stat params **/

      kv = DSET_BRICK_STATCODE(dset,ival) ;
      if( FUNC_IS_STAT(kv) ){
     STATUS("ival c") ;
         outbuf = THD_zzprintf(outbuf,"     statcode = %s",FUNC_prefixstr[kv] ) ;
         npar = FUNC_need_stat_aux[kv] ;
         if( npar > 0 ){
            outbuf = THD_zzprintf(outbuf,";  statpar =") ;
            for( kv=0 ; kv < npar ; kv++ )
               outbuf = THD_zzprintf(outbuf," %g",DSET_BRICK_STATPAR(dset,ival,kv)) ;
         }
         outbuf = THD_zzprintf(outbuf,"\n") ;
     STATUS("ival d") ;
      }

      cpt = DSET_BRICK_KEYWORDS(dset,ival) ;
      if( cpt != NULL && cpt[0] != '\0' ){
        outbuf = THD_zzprintf(outbuf,"     keywords = %.66s\n",cpt) ;
      }

     STATUS("ival z") ;
   }
   if( verbose < 0 && nval_per < dset->dblk->nvals )  /* 21 Sep 2007 */
     outbuf = THD_zzprintf(outbuf,
                "** For info on all %d sub-bricks, use '3dinfo -verb' **\n",
                dset->dblk->nvals) ;

   /** print out dataset global statistical parameters **/

   if( ISFUNC(dset) && FUNC_need_stat_aux[dset->func_type] > 0 ){
      outbuf = THD_zzprintf(outbuf,"Auxiliary functional statistical parameters:\n %s\n",
             FUNC_label_stat_aux[dset->func_type] ) ;
      for( ival=0 ; ival < FUNC_need_stat_aux[dset->func_type] ; ival++ )
         outbuf = THD_zzprintf(outbuf," %g",dset->stat_aux[ival]) ;
      outbuf = THD_zzprintf(outbuf,"\n") ;
   }

   /** If present, print out History **/

   { char *chn ; int j,k ;
     chn = tross_Get_History(dset) ;
     if( chn != NULL ){
       j = strlen(chn) ;
       outbuf = THD_zzprintf(outbuf,"\n----- HISTORY -----\n") ;
       for( k=0 ; k < j ; k += ZMAX )
         outbuf = THD_zzprintf(outbuf,SZMAX,chn+k) ;
       free(chn) ;
       outbuf = THD_zzprintf(outbuf,"\n") ;
     }
   }

   /** If present, print out Notes **/

   if( verbose >= 0 ){
     ATR_int *notecount;
     int num_notes, i, j, mmm ;
     char *chn , *chd ;

     notecount = THD_find_int_atr(dset->dblk, "NOTES_COUNT");
     if( notecount != NULL ){
        num_notes = notecount->in[0] ;
        if( verbose == 0 && num_notes > 5 ) num_notes = 5 ;
        mmm = (verbose > 0) ? ZMAX : 1200 ;   /* 400 it was!
                                                 Come on Bob, have a heart! -ZSS */
        for (i=1; i<= num_notes; i++) {
           chn = tross_Get_Note( dset , i ) ;
           if( chn != NULL ){
              j = strlen(chn) ; if( j > mmm ) chn[mmm] = '\0' ;
              chd = tross_Get_Notedate(dset,i) ;
              if( chd == NULL ){ chd = AFMALL(char,16) ; strcpy(chd,"no date") ; }
              outbuf = THD_zzprintf(outbuf,"\n----- NOTE %d [%s] -----\n%s\n",i,chd,chn) ;
              free(chn) ; free(chd) ;
           }
        }
     }
   }

   RETURN(outbuf) ;
}
