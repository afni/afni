#include "mrilib.h"
#include "thd_patterns.h"
#include <errno.h>
#include "thd_datatable.h"
#include <stdarg.h>

#ifdef USE_SUMA
#include "SUMA_suma.h"
#endif

/*----------------------------------------------------------------------------
  Dataset -> feature vectors.  See thd_patterns.h.
                                                     -- P Molfese, Jul 2026
------------------------------------------------------------------------------*/

THD_roilist * THD_roilist_from_dset( THD_3dim_dataset *mset , char *roi_sel )
{
   THD_roilist *rl ; MRI_IMAGE *mim ; float *mar , fac ;
   int nvox , ii , kk , vv , vmax=0 , nroi=0 ;
   int *seen , *sel=NULL , *v2i ; void *dtbl ;

   if( mset == NULL ) return NULL ;
   DSET_load(mset) ; if( !DSET_LOADED(mset) ) return NULL ;
   nvox = DSET_NVOX(mset) ;

   fac = DSET_BRICK_FACTOR(mset,0) ; if( fac == 0.0f ) fac = 1.0f ;
   mim = mri_to_float( DSET_BRICK(mset,0) ) ; mar = MRI_FLOAT_PTR(mim) ;
   if( fac != 1.0f ) for( ii=0 ; ii < nvox ; ii++ ) mar[ii] *= fac ;

   for( ii=0 ; ii < nvox ; ii++ ){
     vv = (int)rintf(mar[ii]) ; if( vv > vmax ) vmax = vv ;
   }
   if( vmax <= 0 ){ mri_free(mim) ; return NULL ; }

   seen = (int *)calloc(vmax+1,sizeof(int)) ;
   for( ii=0 ; ii < nvox ; ii++ ){
     vv = (int)rintf(mar[ii]) ; if( vv > 0 ) seen[vv]++ ;
   }

   if( roi_sel != NULL ){
     /* MCW_get_intlist wants a count, and allows values 0..count-1 */
     sel = MCW_get_intlist( vmax+1 , roi_sel ) ;
     if( sel == NULL || sel[0] == 0 ){
       free(seen) ; mri_free(mim) ; return NULL ;
     }
     for( vv=1 ; vv <= vmax ; vv++ ){
       int keep=0 ;
       for( ii=1 ; ii <= sel[0] ; ii++ ) if( sel[ii] == vv ){ keep=1 ; break ; }
       if( !keep ) seen[vv] = 0 ;
     }
   }

   for( vv=1 ; vv <= vmax ; vv++ ) if( seen[vv] > 0 ) nroi++ ;
   if( nroi == 0 ){
     free(seen) ; mri_free(mim) ; if( sel != NULL ) free(sel) ; return NULL ;
   }

   rl = (THD_roilist *)calloc(1,sizeof(THD_roilist)) ;
   rl->nroi   = nroi ;
   rl->val    = (int *)malloc(sizeof(int)*nroi) ;
   rl->vox    = (intvec *)calloc(nroi,sizeof(intvec)) ;
   rl->lab    = (char **)calloc(nroi,sizeof(char *)) ;
   rl->center = (int *)malloc(sizeof(int)*nroi) ;
   for( kk=0 ; kk < nroi ; kk++ ) rl->center[kk] = -1 ;   /* paint whole parcel */

   for( kk=0,vv=1 ; vv <= vmax ; vv++ ){
     if( seen[vv] > 0 ){
       rl->val[kk]     = vv ;
       rl->vox[kk].ar  = (int *)malloc(sizeof(int)*seen[vv]) ;
       rl->vox[kk].nar = 0 ;    /* counts up while filling, below */
       kk++ ;
     }
   }

   /* map value -> index once, so filling is O(nvox) and not O(nvox*nroi) */
   v2i = (int *)malloc(sizeof(int)*(vmax+1)) ;
   for( vv=0 ; vv <= vmax ; vv++ ) v2i[vv] = -1 ;
   for( kk=0 ; kk < nroi ; kk++ ) v2i[ rl->val[kk] ] = kk ;

   for( ii=0 ; ii < nvox ; ii++ ){
     vv = (int)rintf(mar[ii]) ;
     if( vv > 0 && vv <= vmax && v2i[vv] >= 0 ){
       intvec *iv = rl->vox + v2i[vv] ;
       iv->ar[ iv->nar++ ] = ii ;
     }
   }

   /* labels, if the mask carries a label table (as AFNI atlases do) */
   dtbl = DSET_Label_Dtable(mset) ;
   if( dtbl != NULL ){
     for( kk=0 ; kk < nroi ; kk++ ){
       char kbuf[32] , *lab ;
       sprintf(kbuf,"%d",rl->val[kk]) ;
       lab = findin_Dtable_a( kbuf , (Dtable *)dtbl ) ;
       if( lab != NULL ) rl->lab[kk] = strdup(lab) ;
     }
   }

   free(v2i) ; free(seen) ; mri_free(mim) ; if( sel != NULL ) free(sel) ;
   return rl ;
}

THD_roilist * THD_roilist_searchlight( THD_3dim_dataset *mset , MCW_cluster *nbhd )
{
   THD_roilist *rl ; MRI_IMAGE *mim ; float *mar , fac ;
   byte *mask ;
   int nvox , nx , ny , nz , nxy , ii , kk , pp , ncen=0 ;

   if( mset == NULL || nbhd == NULL || nbhd->num_pt < 1 ) return NULL ;
   DSET_load(mset) ; if( !DSET_LOADED(mset) ) return NULL ;
   nvox = DSET_NVOX(mset) ;
   nx = DSET_NX(mset) ; ny = DSET_NY(mset) ; nz = DSET_NZ(mset) ; nxy = nx*ny ;

   fac = DSET_BRICK_FACTOR(mset,0) ; if( fac == 0.0f ) fac = 1.0f ;
   mim = mri_to_float( DSET_BRICK(mset,0) ) ; mar = MRI_FLOAT_PTR(mim) ;
   mask = (byte *)calloc(nvox,sizeof(byte)) ;
   for( ii=0 ; ii < nvox ; ii++ )
     if( mar[ii]*fac != 0.0f ){ mask[ii] = 1 ; ncen++ ; }
   mri_free(mim) ;
   if( ncen == 0 ){ free(mask) ; return NULL ; }

   rl = (THD_roilist *)calloc(1,sizeof(THD_roilist)) ;
   rl->nroi   = ncen ;
   rl->val    = (int *)malloc(sizeof(int)*ncen) ;
   rl->vox    = (intvec *)calloc(ncen,sizeof(intvec)) ;
   rl->lab    = (char **)calloc(ncen,sizeof(char *)) ;
   rl->center = (int *)malloc(sizeof(int)*ncen) ;

   for( kk=0,ii=0 ; ii < nvox ; ii++ ){
     int cx , cy , cz , n=0 ;
     if( !mask[ii] ) continue ;
     cz = ii/nxy ; cy = (ii%nxy)/nx ; cx = ii%nx ;

     rl->vox[kk].ar = (int *)malloc(sizeof(int)*nbhd->num_pt) ;
     for( pp=0 ; pp < nbhd->num_pt ; pp++ ){
       int x = cx + nbhd->i[pp] , y = cy + nbhd->j[pp] , z = cz + nbhd->k[pp] , idx ;
       if( x < 0 || x >= nx || y < 0 || y >= ny || z < 0 || z >= nz ) continue ;
       idx = x + y*nx + z*nxy ;
       if( mask[idx] ) rl->vox[kk].ar[n++] = idx ;
     }
     rl->vox[kk].nar = n ;
     rl->val[kk]     = ii ;      /* center voxel index doubles as the ROI id */
     rl->center[kk]  = ii ;
     kk++ ;
   }

   free(mask) ;                  /* caller owns nbhd */
   return rl ;
}

static void thd_patterns_error( char *err, size_t errlen, const char *fmt, ... )
{
   va_list ap ;
   if( err == NULL || errlen == 0 ) return ;
   va_start(ap,fmt) ; vsnprintf(err,errlen,fmt,ap) ; va_end(ap) ;
}

static int thd_parse_shape_one( const char *str, int plen, float *out )
{
   char *ep ; double v ;
   if( str[plen]!='(' ) return 0 ;
   errno=0 ; v=strtod(str+plen+1,&ep) ;
   if( errno==ERANGE || ep==str+plen+1 || *ep!=')' || ep[1]!='\0' ||
       !isfinite(v) || v<=0.0 || v>FLT_MAX ) return 0 ;
   *out=(float)v ; return 1 ;
}

static int thd_parse_shape_three( const char *str, int plen,
                                  float *a, float *b, float *c )
{
   char *ep ; const char *p ; double v[3] ; int ii ;
   if( str[plen]!='(' ) return 0 ; p=str+plen+1 ;
   for( ii=0 ; ii<3 ; ii++ ){
     errno=0 ; v[ii]=strtod(p,&ep) ;
     if( errno==ERANGE || ep==p || !isfinite(v[ii]) || v[ii]<=0.0 || v[ii]>FLT_MAX ) return 0 ;
     if( ii<2 ){ if( *ep!=',' ) return 0 ; p=ep+1 ; }
     else if( *ep!=')' || ep[1]!='\0' ) return 0 ;
   }
   *a=(float)v[0] ; *b=(float)v[1] ; *c=(float)v[2] ; return 1 ;
}

MCW_cluster * THD_searchlight_parse( const char *str, float dx, float dy, float dz,
                                     char *err, size_t errlen )
{
   float a=0.0f , b=0.0f , c=0.0f ;

   if( err != NULL && errlen > 0 ) err[0]='\0' ;
   if( str == NULL || str[0] == '\0' ){
     thd_patterns_error(err,errlen,"empty searchlight neighborhood") ;
     return NULL ;
   }
   if( (str[0] >= '0' && str[0] <= '9') || str[0] == '.' ){
     char *ep ; double v ; errno=0 ; v=strtod(str,&ep) ;
     if( errno==ERANGE || ep==str || *ep!='\0' || !isfinite(v) || v<=0.0 || v>FLT_MAX ){
       thd_patterns_error(err,errlen,"-searchlight radius must be one finite number > 0") ;
       return NULL ;
     }
     a=(float)v ;
     return MCW_spheremask(dx,dy,dz,a) ;
   }
   if( strncasecmp(str,"SPHERE",6) == 0 ){
     if( !thd_parse_shape_one(str,6,&a) ){
       thd_patterns_error(err,errlen,"malformed SPHERE(r): r must be finite and > 0") ;
       return NULL ;
     }
     return MCW_spheremask(dx,dy,dz,a) ;
   }
   if( strncasecmp(str,"RECT",4) == 0 ){
     if( !thd_parse_shape_three(str,4,&a,&b,&c) ){
       thd_patterns_error(err,errlen,"malformed RECT(a,b,c): dimensions must be finite and > 0") ;
       return NULL ;
     }
     return MCW_rectmask(dx,dy,dz,a,b,c) ;
   }
   if( strncasecmp(str,"RHDD",4) == 0 ){
     if( !thd_parse_shape_one(str,4,&a) ){
       thd_patterns_error(err,errlen,"malformed RHDD(r): r must be finite and > 0") ;
       return NULL ;
     }
     return MCW_rhddmask(dx,dy,dz,a) ;
   }
   if( strncasecmp(str,"TOHD",4) == 0 ){
     if( !thd_parse_shape_one(str,4,&a) ){
       thd_patterns_error(err,errlen,"malformed TOHD(r): r must be finite and > 0") ;
       return NULL ;
     }
     return MCW_tohdmask(dx,dy,dz,a) ;
   }
   thd_patterns_error(err,errlen,
                      "unknown -searchlight neighborhood '%s'.  Use a radius in\n"
                      "       mm, or SPHERE(r) / RECT(a,b,c) / RHDD(r) / TOHD(r).",
                      str) ;
   return NULL ;
}

void THD_roilist_paint( float *ar, const THD_roilist *rl, const float *vals )
{
   int kk,ii ;
   if( ar == NULL || rl == NULL || vals == NULL ) return ;
   for( kk=0 ; kk<rl->nroi ; kk++ ){
     if( rl->center[kk] >= 0 ) ar[rl->center[kk]]=vals[kk] ;
     else for( ii=0 ; ii<rl->vox[kk].nar ; ii++ )
       ar[rl->vox[kk].ar[ii]]=vals[kk] ;
   }
}

#ifdef USE_SUMA
THD_roilist * THD_roilist_searchlight_surf( const char *surf_file,
                                             THD_3dim_dataset *mset,
                                             float radius, int all_nodes,
                                             char *err, size_t errlen )
{
   SUMA_SurfaceObject *SO ;
   SUMA_GET_OFFSET_STRUCT *OffS ;
   THD_roilist *rl ;
   MRI_IMAGE *mim ; float *mar , fac ;
   byte *mask ; int *n2i , *tmp ;
   int nvox = DSET_NVOX(mset) , nnode , sparse , ncen=0 , m , kk , ii ;

   if( err != NULL && errlen > 0 ) err[0]='\0' ;
   { SUMA_SO_File_Type ft = SUMA_GuessSurfFormatFromExtension( (char *)surf_file , NULL ) ;
     if( ft == SUMA_FT_NOT_SPECIFIED || ft == SUMA_FT_ERROR ){
       thd_patterns_error(err,errlen,
                          "can't tell the surface type of '%s' from its name\n"
                          "       (use a .gii / .asc / .ply / ... extension)",
                          surf_file) ;
       return NULL ;
     }
     SO = SUMA_Load_Surface_Object_Wrapper( (char *)surf_file , NULL , NULL , ft,
                                            SUMA_FF_NOT_SPECIFIED , NULL , 0 ) ;
   }
   if( SO == NULL ){
     thd_patterns_error(err,errlen,"can't read -surf surface '%s'",surf_file) ;
     return NULL ;
   }
   if( !SO->EL || !SO->FN )
     SUMA_SurfaceMetrics_eng( SO , "EdgeList" , NULL , 0 , SUMAg_CF->DsetList ) ;
   if( !SO->FN ){
     thd_patterns_error(err,errlen,
                        "surface '%s' has no node connectivity (no mesh?)",surf_file) ;
     return NULL ;
   }
   nnode = SO->N_Node ;

   sparse = ( ( DBLK_IS_NI_SURF_DSET(mset->dblk) || DBLK_IS_GIFTI(mset->dblk) ) &&
              mset->dblk->nnodes > 0 && mset->dblk->node_list != NULL ) ;
   n2i = (int *)malloc(sizeof(int)*nnode) ;
   for( m=0 ; m<nnode ; m++ ) n2i[m]=-1 ;
   if( sparse ){
     for( ii=0 ; ii<mset->dblk->nnodes ; ii++ ){
       int nid=mset->dblk->node_list[ii] ;
       if( nid>=0 && nid<nnode ) n2i[nid]=ii ;
     }
   } else {
     int nn=(nvox<nnode) ? nvox : nnode ;
     for( ii=0 ; ii<nn ; ii++ ) n2i[ii]=ii ;
   }

   mask=(byte *)calloc(nvox,sizeof(byte)) ;
   if( all_nodes ){
     for( ii=0 ; ii<nvox ; ii++ ) mask[ii]=1 ;
   } else {
     fac=DSET_BRICK_FACTOR(mset,0) ; if( fac==0.0f ) fac=1.0f ;
     mim=mri_to_float(DSET_BRICK(mset,0)) ; mar=MRI_FLOAT_PTR(mim) ;
     for( ii=0 ; ii<nvox ; ii++ ) if( mar[ii]*fac != 0.0f ) mask[ii]=1 ;
     mri_free(mim) ;
   }

   for( m=0 ; m<nnode ; m++ ) if( n2i[m]>=0 && mask[n2i[m]] ) ncen++ ;
   if( ncen==0 ){ free(n2i) ; free(mask) ; return NULL ; }

   rl=(THD_roilist *)calloc(1,sizeof(THD_roilist)) ;
   rl->nroi=ncen ;
   rl->val=(int *)malloc(sizeof(int)*ncen) ;
   rl->vox=(intvec *)calloc(ncen,sizeof(intvec)) ;
   rl->lab=(char **)calloc(ncen,sizeof(char *)) ;
   rl->center=(int *)malloc(sizeof(int)*ncen) ;

   OffS=SUMA_Initialize_getoffsets(nnode) ;
   tmp=(int *)malloc(sizeof(int)*nnode) ;

   for( kk=0,m=0 ; m<nnode ; m++ ){
     int ci=(n2i[m]>=0) ? n2i[m] : -1 , il,jl,n ;
     if( ci<0 || !mask[ci] ) continue ;

     SUMA_getoffsets2(m,SO,radius,OffS,NULL,0) ;
     n=0 ; tmp[n++]=ci ;
     for( il=1 ; il<OffS->N_layers ; il++ )
       for( jl=0 ; jl<OffS->layers[il].N_NodesInLayer ; jl++ ){
         int nb=OffS->layers[il].NodesInLayer[jl] ;
         if( OffS->OffVect[nb]<=radius && n2i[nb]>=0 && mask[n2i[nb]] )
           tmp[n++]=n2i[nb] ;
       }
     SUMA_Recycle_getoffsets(OffS) ;

     rl->vox[kk].ar=(int *)malloc(sizeof(int)*n) ;
     memcpy(rl->vox[kk].ar,tmp,sizeof(int)*n) ;
     rl->vox[kk].nar=n ;
     rl->val[kk]=ci ; rl->center[kk]=ci ; kk++ ;
   }

   SUMA_Free_getoffsets(OffS) ;
   free(tmp) ; free(n2i) ; free(mask) ;
   return rl ;
}
#endif /* USE_SUMA */

void THD_roilist_free( THD_roilist *rl )
{
   int kk ;
   if( rl == NULL ) return ;
   for( kk=0 ; kk < rl->nroi ; kk++ ){
     if( rl->vox[kk].ar != NULL ) free(rl->vox[kk].ar) ;
     if( rl->lab[kk]    != NULL ) free(rl->lab[kk]) ;
   }
   free(rl->vox) ; free(rl->lab) ; free(rl->val) ;
   if( rl->center != NULL ) free(rl->center) ;
   free(rl) ;
}

int THD_roilist_maxvox( THD_roilist *rl )
{
   int kk , mx=0 ;
   if( rl == NULL ) return 0 ;
   for( kk=0 ; kk < rl->nroi ; kk++ )
     if( rl->vox[kk].nar > mx ) mx = rl->vox[kk].nar ;
   return mx ;
}

/*----------------------------------------------------------------------------*/

void THD_roi_mean_ts( THD_3dim_dataset *dset , intvec *vox ,
                      int polort , float *out )
{
   int nvals = DSET_NVALS(dset) , ii , tt ;

   for( tt=0 ; tt < nvals ; tt++ ) out[tt] = 0.0f ;
   if( vox->nar <= 0 ) return ;

   /* Dispatch once per brick and sum its typed array directly.  The previous
      THD_extract_array path allocated/freed a complete time-series buffer for
      every voxel, which dominated atlas preprocessing on dense 2 mm data. */
   for( tt=0 ; tt<nvals ; tt++ ){
     void *ar=DSET_ARRAY(dset,tt) ; float fac=DSET_BRICK_FACTOR(dset,tt) ;
     if( ar==NULL ) continue ;
     if( fac<=0.0f ) fac=1.0f ;
#define ACCUM_ROI_MEAN(type,expr)                                                \
     do { type *src=(type *)ar ;                                                \
          if( fac==1.0f ) for( ii=0 ; ii<vox->nar ; ii++ )                     \
            out[tt]+=(float)(expr) ;                                            \
          else for( ii=0 ; ii<vox->nar ; ii++ )                               \
            out[tt]+=(float)(expr)*fac ;                                        \
     } while(0)
     switch( DSET_BRICK_TYPE(dset,tt) ){
       case MRI_byte:    ACCUM_ROI_MEAN(byte,   src[vox->ar[ii]]) ; break ;
       case MRI_short:   ACCUM_ROI_MEAN(short,  src[vox->ar[ii]]) ; break ;
       case MRI_int:     ACCUM_ROI_MEAN(int,    src[vox->ar[ii]]) ; break ;
       case MRI_float:   ACCUM_ROI_MEAN(float,  src[vox->ar[ii]]) ; break ;
       case MRI_double:  ACCUM_ROI_MEAN(double, src[vox->ar[ii]]) ; break ;
       case MRI_complex: ACCUM_ROI_MEAN(complex,CABS(src[vox->ar[ii]])) ; break ;
       default: break ;
     }
#undef ACCUM_ROI_MEAN
     out[tt] /= (float)vox->nar ;
   }

   /* detrending the mean is the same as averaging detrended voxels */
   if( polort >= 0 )
     (void)THD_generic_detrend_LSQ( nvals , out , polort , 0 , NULL , NULL ) ;
}

void THD_roi_pattern( THD_3dim_dataset *dset , intvec *vox , float *out )
{
   int nvals = DSET_NVALS(dset) , nvx = vox->nar , ii , bb ;

   /* THD_get_voxel performs dataset/type/bounds dispatch for every value.
      These voxel indices have already been validated while the ROI list was
      built, so dispatch once per brick and walk its typed array directly. */
   for( bb=0 ; bb < nvals ; bb++ ){
     void *ar = DSET_ARRAY(dset,bb) ;
     float *dst = out + (size_t)bb*nvx ;
     float fac = DSET_BRICK_FACTOR(dset,bb) ;

     if( ar == NULL ){ DSET_load(dset) ; ar = DSET_ARRAY(dset,bb) ; }
     if( ar == NULL ){
       for( ii=0 ; ii < nvx ; ii++ ) dst[ii] = 0.0f ;
       continue ;
     }
     if( fac <= 0.0f ) fac = 1.0f ;       /* match THD_get_voxel exactly */

#define COPY_SCALAR_PATTERN(type)                                                \
     do { type *src=(type *)ar ;                                                 \
          if( fac == 1.0f )                                                      \
            for( ii=0 ; ii < nvx ; ii++ ) dst[ii]=(float)src[vox->ar[ii]] ;      \
          else                                                                   \
            for( ii=0 ; ii < nvx ; ii++ )                                       \
              dst[ii]=(float)src[vox->ar[ii]]*fac ;                              \
     } while(0)

     switch( DSET_BRICK_TYPE(dset,bb) ){
       default:
         for( ii=0 ; ii < nvx ; ii++ ) dst[ii] = 0.0f ;
       break ;

       case MRI_byte:   COPY_SCALAR_PATTERN(byte)   ; break ;
       case MRI_short:  COPY_SCALAR_PATTERN(short)  ; break ;
       case MRI_int:    COPY_SCALAR_PATTERN(int)    ; break ;
       case MRI_float:  COPY_SCALAR_PATTERN(float)  ; break ;
       case MRI_double: COPY_SCALAR_PATTERN(double) ; break ;

       case MRI_complex:{
         complex *src=(complex *)ar ;
         for( ii=0 ; ii < nvx ; ii++ ){
           complex c=src[vox->ar[ii]] ;
           dst[ii]=sqrtf(c.r*c.r+c.i*c.i)*fac ;
         }
       }
       break ;

       case MRI_rgb:{
         rgbyte *src=(rgbyte *)ar ;
         for( ii=0 ; ii < nvx ; ii++ ){
           rgbyte c=src[vox->ar[ii]] ;
           dst[ii]=(0.299f*(float)c.r + 0.587f*(float)c.g
                                  + 0.114f*(float)c.b)*fac ;
         }
       }
       break ;

       case MRI_rgba:{
         rgba *src=(rgba *)ar ;
         for( ii=0 ; ii < nvx ; ii++ ){
           rgba c=src[vox->ar[ii]] ;
           float val=0.299f*(float)c.r + 0.587f*(float)c.g
                                        + 0.114f*(float)c.b ;
           dst[ii]=val*(0.00392157f*(float)c.a)*fac ;
         }
       }
       break ;
     }
#undef COPY_SCALAR_PATTERN
   }
}

/*============================================================================*/
/*  Run-aware input for cross-validated classic RSA.  See thd_patterns.h.      */
/*============================================================================*/

static int condlab_cmp( const void *a, const void *b )
{
   char *aa=*(char * const *)a, *bb=*(char * const *)b ;
   return strcmp(aa,bb) ;
}

/*! Read exactly want whitespace-free condition labels, ignoring blank lines
    and comments.  The caller owns the returned strings and vector. */
static char ** read_condition_file( char *fname, int want )
{
   FILE *fp ; char **lab ; char line[4096],tok[512],extra[2] ; int n=0,ii ;
   if( fname==NULL || want<1 ) return NULL ;
   fp=fopen(fname,"r") ;
   if( fp==NULL ){
     ERROR_message("runwiseTable: cannot open ConditionFile '%s'",fname) ;
     return NULL ;
   }
   lab=(char **)calloc(want,sizeof(char *)) ;
   if( lab==NULL ){ fclose(fp) ; return NULL ; }
   while( fgets(line,sizeof(line),fp)!=NULL ){
     char *p=line,*hash ;
     while( isspace((unsigned char)*p) ) p++ ;
     if( *p=='\0' || *p=='#' ) continue ;
     hash=strchr(p,'#') ; if( hash!=NULL ) *hash='\0' ;
     if( sscanf(p,"%511s %1s",tok,extra)!=1 ){
       ERROR_message("runwiseTable: ConditionFile '%s' needs one label per line",fname) ;
       goto bad ;
     }
     if( n>=want ){
       ERROR_message("runwiseTable: ConditionFile '%s' has more labels than its %d InputFile bricks",
                     fname,want) ;
       goto bad ;
     }
     lab[n]=strdup(tok) ; if( lab[n]==NULL ) goto bad ; n++ ;
   }
   fclose(fp) ; fp=NULL ;
   if( n!=want ){
     ERROR_message("runwiseTable: ConditionFile '%s' has %d labels; its InputFile has %d bricks",
                   fname,n,want) ;
     goto bad ;
   }
   return lab ;
bad:
   if( fp!=NULL ) fclose(fp) ;
   for( ii=0 ; ii<want ; ii++ ) free(lab[ii]) ;
   free(lab) ;
   return NULL ;
}

/*! Read the S6 descriptor for one subject x run beta-series dataset. The file
    is a strict two-column table:

        Trial       Condition
        trial_001   face
        trial_002   house

    Rows are in InputFile sub-brick order. Subject and Run deliberately stay in
    the containing runwiseTable so that they cannot disagree with this local
    descriptor. The caller owns both returned string vectors. */
static int read_trial_file( char *fname, int want,
                            char ***trial_out, char ***cond_out )
{
   THD_datatable *td ; char **trial=NULL,**cond=NULL ;
   int it,ic,ii,ok=1 ;
   if( trial_out!=NULL ) *trial_out=NULL ;
   if( cond_out !=NULL ) *cond_out =NULL ;
   if( fname==NULL || want<1 || trial_out==NULL || cond_out==NULL ) return 0 ;
   td=THD_read_datatable_file(fname) ;
   if( td==NULL ){
     ERROR_message("runwiseTable: cannot read TrialFile '%s'",fname) ; return 0 ;
   }
   it=THD_datatable_column(td,"Trial") ;
   ic=THD_datatable_column(td,"Condition") ;
   if( it<0 || ic<0 ){
     ERROR_message("runwiseTable: TrialFile '%s' needs Trial and Condition columns",fname) ;
     ok=0 ;
   } else if( td->ncol!=2 ){
     ERROR_message("runwiseTable: TrialFile '%s' has %d columns; the first contract needs\n"
                   "       exactly Trial and Condition",fname,td->ncol) ;
     ok=0 ;
   } else if( td->nrow!=want ){
     ERROR_message("runwiseTable: TrialFile '%s' has %d trial rows; its InputFile has %d bricks",
                   fname,td->nrow,want) ;
     ok=0 ;
   }
   if( ok ){
     trial=(char **)calloc(want,sizeof(char *)) ;
     cond =(char **)calloc(want,sizeof(char *)) ;
     if( trial==NULL || cond==NULL ) ok=0 ;
   }
   for( ii=0 ; ii<want && ok ; ii++ ){
     trial[ii]=strdup(DT_CELL(td,ii,it)) ;
     cond[ii] =strdup(DT_CELL(td,ii,ic)) ;
     if( trial[ii]==NULL || cond[ii]==NULL ) ok=0 ;
   }
   THD_free_datatable(td) ;
   if( !ok ){
     if( trial!=NULL ) for( ii=0 ; ii<want ; ii++ ) free(trial[ii]) ;
     if( cond !=NULL ) for( ii=0 ; ii<want ; ii++ ) free(cond[ii]) ;
     free(trial) ; free(cond) ; return 0 ;
   }
   *trial_out=trial ; *cond_out=cond ; return 1 ;
}

THD_runset * THD_runset_read( char *fname )
{
   THD_datatable *dt ; THD_runset *rs ;
   int iRun , iResid , iCond , iTrial , i , s , r , r2 , ok=1 ;

   if( fname == NULL ) return NULL ;
   dt = THD_read_datatable_file( fname ) ;
   if( dt == NULL ){ ERROR_message("runwiseTable: cannot read '%s'",fname) ; return NULL ; }

   if( dt->icol_subj < 0 || dt->icol_input < 0 ){
     ERROR_message("runwiseTable '%s' needs Subj and InputFile columns",fname) ;
     THD_free_datatable(dt) ; return NULL ;
   }
   iRun   = THD_datatable_column( dt , "Run" ) ;
   iResid = THD_datatable_column( dt , "ResidFile" ) ;
   iCond  = THD_datatable_column( dt , "ConditionFile" ) ;
   iTrial = THD_datatable_column( dt , "TrialFile" ) ;
   if( iRun < 0 ){
     ERROR_message("runwiseTable '%s' needs a 'Run' column (one row per subject x run)",fname) ;
     THD_free_datatable(dt) ; return NULL ;
   }
   if( iCond>=0 && iTrial>=0 ){
     ERROR_message("runwiseTable '%s' cannot contain both ConditionFile and TrialFile;\n"
                   "       TrialFile already supplies each trial's condition",fname) ;
     THD_free_datatable(dt) ; return NULL ;
   }

   rs = (THD_runset *)calloc(1,sizeof(THD_runset)) ;
   rs->nrow      = dt->nrow ;
   rs->has_resid = (iResid >= 0) ;
   rs->has_condmap = (iCond >= 0 || iTrial >= 0) ;
   rs->has_trialmap= (iTrial >= 0) ;
   rs->source    = strdup(fname) ;
   rs->row_sub   = (int   *)malloc(sizeof(int)*rs->nrow) ;
   rs->run_lab   = (char **)calloc(rs->nrow,sizeof(char *)) ;
   rs->betas     = (THD_3dim_dataset **)calloc(rs->nrow,sizeof(THD_3dim_dataset *)) ;
   rs->resid     = rs->has_resid ?
                   (THD_3dim_dataset **)calloc(rs->nrow,sizeof(THD_3dim_dataset *)) : NULL ;
   rs->subj      = (char **)calloc(rs->nrow,sizeof(char *)) ;   /* <= nrow subjects */
   rs->nrun      = (int   *)calloc(rs->nrow,sizeof(int)) ;
   rs->nbrick    = (int   *)calloc(rs->nrow,sizeof(int)) ;
   if( rs->has_condmap )
     rs->cond_of = (int **)calloc(rs->nrow,sizeof(int *)) ;
   if( rs->has_trialmap )
     rs->trial_lab = (char ***)calloc(rs->nrow,sizeof(char **)) ;

   /* group rows by subject, in first-seen order */
   rs->nsub = 0 ;
   for( i=0 ; i < dt->nrow ; i++ ){
     char *sj = dt->subj[i] ; int si=-1 ;
     for( s=0 ; s < rs->nsub ; s++ ) if( strcmp(rs->subj[s],sj)==0 ){ si=s ; break ; }
     if( si < 0 ){ si = rs->nsub ; rs->subj[si] = strdup(sj) ; rs->nsub++ ; }
     rs->row_sub[i] = si ; rs->nrun[si]++ ;
     rs->run_lab[i] = strdup( DT_CELL(dt,i,iRun) ) ;
   }

   /* per-subject list of row indices */
   rs->row_of = (int **)calloc(rs->nsub,sizeof(int *)) ;
   { int *fill = (int *)calloc(rs->nsub,sizeof(int)) ;
     for( s=0 ; s < rs->nsub ; s++ ) rs->row_of[s] = (int *)malloc(sizeof(int)*rs->nrun[s]) ;
     for( i=0 ; i < dt->nrow ; i++ ){ s = rs->row_sub[i] ; rs->row_of[s][ fill[s]++ ] = i ; }
     free(fill) ;
   }

   /* Open dataset headers and validate the grid. The old path retains its
      common brick-count contract. ConditionFile or TrialFile maps local bricks
      into a global label set and therefore permits different counts/order. */
   rs->ncond = rs->has_condmap ? 0 : -1 ;
   rs->nvox = -1 ; rs->resid_nt = 0 ; rs->maxbrick=0 ;
   for( i=0 ; i < dt->nrow && ok ; i++ ){
     THD_3dim_dataset *ds = THD_open_dataset( dt->fname[i] ) ;
     if( ds == NULL ){
       ERROR_message("runwiseTable: can't open InputFile '%s' (row %d)",dt->fname[i],i+1) ;
       ok=0 ; break ;
     }
     rs->betas[i] = ds ;
     rs->nbrick[i]=DSET_NVALS(ds) ;
     if( rs->nbrick[i]>rs->maxbrick ) rs->maxbrick=rs->nbrick[i] ;
     if( rs->nvox < 0 ) rs->nvox = DSET_NVOX(ds) ;
     else if( DSET_NVOX(ds) != rs->nvox || !EQUIV_GRIDS(ds,rs->betas[0]) ){
       ERROR_message("runwiseTable: '%s' is on a different grid than the first row",
                     dt->fname[i]) ; ok=0 ; break ;
     }
     if( !rs->has_condmap ){
       if( rs->ncond<0 ) rs->ncond=rs->nbrick[i] ;
       else if( rs->nbrick[i] != rs->ncond ){
         ERROR_message("runwiseTable: '%s' has %d conditions but the first row has %d",
                       dt->fname[i],rs->nbrick[i],rs->ncond) ; ok=0 ; break ;
       }
     } else {
       char **lab=NULL,**tlab=NULL ; int bb ;
       if( rs->has_trialmap ){
         if( !read_trial_file(DT_CELL(dt,i,iTrial),rs->nbrick[i],&tlab,&lab) ){
           ok=0 ; break ;
         }
         rs->trial_lab[i]=tlab ;
       } else {
         lab=read_condition_file(DT_CELL(dt,i,iCond),rs->nbrick[i]) ;
       }
       if( lab==NULL ){ ok=0 ; break ; }
       rs->cond_of[i]=(int *)malloc(sizeof(int)*rs->nbrick[i]) ;
       if( rs->cond_of[i]==NULL ){
         for( bb=0 ; bb<rs->nbrick[i] ; bb++ ) free(lab[bb]) ;
         free(lab) ; ok=0 ; break ;
       }
       for( bb=0 ; bb<rs->nbrick[i] ; bb++ ){
         int cc ;
         for( cc=0 ; cc<rs->ncond ; cc++ )
           if( strcmp(rs->cond_lab[cc],lab[bb])==0 ) break ;
         if( cc==rs->ncond ){
           char **tmp=(char **)realloc(rs->cond_lab,sizeof(char *)*(rs->ncond+1)) ;
           if( tmp==NULL ){ ok=0 ; break ; }
           rs->cond_lab=tmp ; rs->cond_lab[rs->ncond]=strdup(lab[bb]) ;
           if( rs->cond_lab[rs->ncond]==NULL ){ ok=0 ; break ; }
           rs->ncond++ ;
         }
         rs->cond_of[i][bb]=cc ;
       }
       for( bb=0 ; bb<rs->nbrick[i] ; bb++ ) free(lab[bb]) ;
       free(lab) ; if( !ok ) break ;
     }
     if( rs->has_resid ){
       char *rf = DT_CELL(dt,i,iResid) ;
       THD_3dim_dataset *rd = THD_open_dataset( rf ) ; int nt ;
       if( rd == NULL ){
         ERROR_message("runwiseTable: can't open ResidFile '%s' (row %d)",rf,i+1) ; ok=0 ; break ;
       }
       if( DSET_NVOX(rd) != rs->nvox || !EQUIV_GRIDS(rd,rs->betas[0]) ){
         ERROR_message("runwiseTable: ResidFile '%s' is on a different grid than InputFile",rf) ;
         DSET_delete(rd) ; ok=0 ; break ;
       }
       nt = DSET_NVALS(rd) ;
       if( rs->resid_nt == 0 || nt < rs->resid_nt ) rs->resid_nt = nt ;
       rs->resid[i] = rd ;
     }
   }

   /* Trial identity belongs to a subject, not merely one run. Catch duplicated
      labels across that subject's complete beta series before any aggregation
      can hide an accidental repeated/misaligned beta. The same trial label may
      occur for different subjects. */
   if( ok && rs->has_trialmap ) for( s=0 ; s<rs->nsub && ok ; s++ ){
     int ra,rb,ba,bb ;
     for( ra=0 ; ra<rs->nrun[s] && ok ; ra++ ){
       int rowa=rs->row_of[s][ra] ;
       for( ba=0 ; ba<rs->nbrick[rowa] && ok ; ba++ ){
         for( rb=ra ; rb<rs->nrun[s] && ok ; rb++ ){
           int rowb=rs->row_of[s][rb] ; int b0=(rb==ra)?ba+1:0 ;
           for( bb=b0 ; bb<rs->nbrick[rowb] ; bb++ )
             if( strcmp(rs->trial_lab[rowa][ba],rs->trial_lab[rowb][bb])==0 ){
               ERROR_message("runwiseTable: subject '%s' has duplicate Trial ID '%s' in runs\n"
                             "       '%s' and '%s'; trial IDs must be unique within subject",
                             rs->subj[s],rs->trial_lab[rowa][ba],
                             rs->run_lab[rowa],rs->run_lab[rowb]) ;
               ok=0 ; break ;
             }
         }
       }
     }
   }

   /* Canonical F21 model/RDM order is lexical and therefore independent of
      table row order.  Remap every local brick, then materialize repetition
      counts used both for within-run averaging and pairwise run eligibility. */
   if( ok && rs->has_condmap ){
     char **old=rs->cond_lab,**sorted ; int *old2new,cc,bb ;
     if( rs->ncond<2 ){
       ERROR_message("runwiseTable: %s mappings define only %d condition; need at least 2",
                     rs->has_trialmap?"TrialFile":"ConditionFile",rs->ncond) ; ok=0 ;
     } else {
       sorted=(char **)malloc(sizeof(char *)*rs->ncond) ;
       old2new=(int *)malloc(sizeof(int)*rs->ncond) ;
       if( sorted==NULL || old2new==NULL ){ free(sorted) ; free(old2new) ; ok=0 ; }
       else {
         memcpy(sorted,old,sizeof(char *)*rs->ncond) ;
         qsort(sorted,rs->ncond,sizeof(char *),condlab_cmp) ;
         for( cc=0 ; cc<rs->ncond ; cc++ ){
           int nn ; for( nn=0 ; nn<rs->ncond ; nn++ ) if( old[cc]==sorted[nn] ) break ;
           old2new[cc]=nn ;
         }
         for( i=0 ; i<rs->nrow ; i++ )
           for( bb=0 ; bb<rs->nbrick[i] ; bb++ )
             rs->cond_of[i][bb]=old2new[rs->cond_of[i][bb]] ;
         free(old) ; rs->cond_lab=sorted ; free(old2new) ;
         rs->nrep=(int **)calloc(rs->nrow,sizeof(int *)) ;
         if( rs->nrep==NULL ) ok=0 ;
         for( i=0 ; i<rs->nrow && ok ; i++ ){
           rs->nrep[i]=(int *)calloc(rs->ncond,sizeof(int)) ;
           if( rs->nrep[i]==NULL ){ ok=0 ; break ; }
           for( bb=0 ; bb<rs->nbrick[i] ; bb++ ) rs->nrep[i][rs->cond_of[i][bb]]++ ;
         }
       }
     }
   }

   /* >= 2 runs per subject, and unique run labels within a subject */
   for( s=0 ; s < rs->nsub && ok ; s++ ){
     if( rs->nrun[s] < 2 ){
       ERROR_message("runwiseTable: subject '%s' has only %d run; cross-validation needs >= 2",
                     rs->subj[s],rs->nrun[s]) ; ok=0 ; break ;
     }
     for( r=0 ; r < rs->nrun[s] && ok ; r++ )
       for( r2=r+1 ; r2 < rs->nrun[s] ; r2++ )
         if( strcmp(rs->run_lab[rs->row_of[s][r]],rs->run_lab[rs->row_of[s][r2]])==0 ){
           ERROR_message("runwiseTable: subject '%s' has two runs labeled '%s'; run labels\n"
                         "       must be unique within a subject",
                         rs->subj[s],rs->run_lab[rs->row_of[s][r]]) ; ok=0 ; break ;
         }
   }

   /* A complete subject RDM is required downstream.  Missing conditions are
      therefore allowed per run, but every pair must have two independent runs
      in which both members occur. */
   if( ok && rs->has_condmap ) for( s=0 ; s<rs->nsub && ok ; s++ ){
     int a,b ;
     for( a=0 ; a<rs->ncond && ok ; a++ ) for( b=a+1 ; b<rs->ncond ; b++ ){
       int nv=0 ;
       for( r=0 ; r<rs->nrun[s] ; r++ ){
         int row=rs->row_of[s][r] ;
         if( rs->nrep[row][a]>0 && rs->nrep[row][b]>0 ) nv++ ;
       }
       if( nv<2 ){
         ERROR_message("runwiseTable: subject '%s' has only %d run%s containing both conditions\n"
                       "       '%s' and '%s'; crossnobis needs at least 2",
                       rs->subj[s],nv,(nv==1)?"":"s",rs->cond_lab[a],rs->cond_lab[b]) ;
         ok=0 ; break ;
       }
     }
   }

   if( ok && rs->has_resid && rs->resid_nt <= rs->ncond )
     WARNING_message("runwiseTable: residual series have %d time points for %d conditions;\n"
                     "       covariance whitening may be ill-conditioned",
                     rs->resid_nt,rs->ncond) ;

   THD_free_datatable(dt) ;
   if( !ok ){ THD_runset_free(rs) ; return NULL ; }
   return rs ;
}

void THD_runset_free( THD_runset *rs )
{
   int i , s ;
   if( rs == NULL ) return ;
   if( rs->betas != NULL ){
     for( i=0 ; i < rs->nrow ; i++ ) if( rs->betas[i] ) DSET_delete(rs->betas[i]) ;
     free(rs->betas) ;
   }
   if( rs->resid != NULL ){
     for( i=0 ; i < rs->nrow ; i++ ) if( rs->resid[i] ) DSET_delete(rs->resid[i]) ;
     free(rs->resid) ;
   }
   if( rs->run_lab != NULL ){
     for( i=0 ; i < rs->nrow ; i++ ) if( rs->run_lab[i] ) free(rs->run_lab[i]) ;
     free(rs->run_lab) ;
   }
   if( rs->subj != NULL ){
     for( s=0 ; s < rs->nsub ; s++ ) if( rs->subj[s] ) free(rs->subj[s]) ;
     free(rs->subj) ;
   }
   if( rs->row_of != NULL ){
     for( s=0 ; s < rs->nsub ; s++ ) if( rs->row_of[s] ) free(rs->row_of[s]) ;
     free(rs->row_of) ;
   }
   if( rs->cond_of != NULL ){
     for( i=0 ; i<rs->nrow ; i++ ) free(rs->cond_of[i]) ;
     free(rs->cond_of) ;
   }
   if( rs->nrep != NULL ){
     for( i=0 ; i<rs->nrow ; i++ ) free(rs->nrep[i]) ;
     free(rs->nrep) ;
   }
   if( rs->trial_lab != NULL ){
     for( i=0 ; i<rs->nrow ; i++ ){
       if( rs->trial_lab[i]!=NULL ){
         int bb ; for( bb=0 ; bb<rs->nbrick[i] ; bb++ ) free(rs->trial_lab[i][bb]) ;
         free(rs->trial_lab[i]) ;
       }
     }
     free(rs->trial_lab) ;
   }
   if( rs->cond_lab != NULL ){
     for( i=0 ; i<rs->ncond ; i++ ) free(rs->cond_lab[i]) ;
     free(rs->cond_lab) ;
   }
   if( rs->row_sub != NULL ) free(rs->row_sub) ;
   if( rs->nrun    != NULL ) free(rs->nrun) ;
   if( rs->nbrick  != NULL ) free(rs->nbrick) ;
   if( rs->source  != NULL ) free(rs->source) ;
   free(rs) ;
}

void THD_runset_print( THD_runset *rs , FILE *fp )
{
   int s , r ;
   if( rs == NULL || fp == NULL ) return ;
   fprintf(fp,"# runwiseTable: %s\n",(rs->source!=NULL)?rs->source:"?") ;
   fprintf(fp,"#   subjects   : %d\n",rs->nsub) ;
   if( rs->has_condmap ){
     int ii,minb=INT_MAX,maxb=0 ;
     for( ii=0 ; ii<rs->nrow ; ii++ ){
       if( rs->nbrick[ii]<minb ) minb=rs->nbrick[ii] ;
       if( rs->nbrick[ii]>maxb ) maxb=rs->nbrick[ii] ;
     }
     if( rs->has_trialmap )
       fprintf(fp,"#   conditions : %d named (TrialFile; %d..%d trial betas/run; trials averaged)\n",
               rs->ncond,minb,maxb) ;
     else
       fprintf(fp,"#   conditions : %d named (ConditionFile; %d..%d bricks/run; repeats averaged)\n",
               rs->ncond,minb,maxb) ;
     fprintf(fp,"#   cond order :") ;
     for( ii=0 ; ii<rs->ncond ; ii++ ) fprintf(fp," %s",rs->cond_lab[ii]) ;
     fprintf(fp,"\n") ;
   } else {
     fprintf(fp,"#   conditions : %d (sub-bricks per InputFile)\n",rs->ncond) ;
   }
   fprintf(fp,"#   voxels     : %d\n",rs->nvox) ;
   fprintf(fp,"#   residuals  : %s",rs->has_resid?"yes":"no (unwhitened crossnobis)") ;
   if( rs->has_resid ) fprintf(fp," (min %d time points)",rs->resid_nt) ;
   fprintf(fp,"\n") ;
   for( s=0 ; s < rs->nsub ; s++ ){
     fprintf(fp,"#   %-12s %d runs:",rs->subj[s],rs->nrun[s]) ;
     for( r=0 ; r < rs->nrun[s] ; r++ )
       fprintf(fp," %s",rs->run_lab[ rs->row_of[s][r] ]) ;
     fprintf(fp,"\n") ;
   }
}
