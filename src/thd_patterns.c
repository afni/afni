#include "mrilib.h"
#include "thd_patterns.h"
#include "thd_xmat.h"
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

static int thd_patterns_is_surface( const THD_3dim_dataset *dset )
{
   return dset!=NULL && ISVALID_DBLK(dset->dblk) &&
          (DBLK_IS_NI_SURF_DSET(dset->dblk) || DBLK_IS_GIFTI(dset->dblk)) ;
}

static int thd_node_id_cmp( const void *aa, const void *bb )
{
   int a=*(const int *)aa,b=*(const int *)bb ;
   return (a>b)-(a<b) ;
}

int THD_surface_domain_match( const THD_3dim_dataset *aa,
                              const THD_3dim_dataset *bb,
                              char *err, size_t errlen )
{
   int ii,na,nb,sa,sb,ordered=1 ;
   if( err!=NULL && errlen>0 ) err[0]='\0' ;
   if( !thd_patterns_is_surface(aa) || !thd_patterns_is_surface(bb) ){
     thd_patterns_error(err,errlen,
                        "surface inputs must both be NIML/GIFTI surface datasets") ;
     return 0 ;
   }
   na=DSET_NVOX(aa) ; nb=DSET_NVOX(bb) ;
   if( na!=nb ){
     thd_patterns_error(err,errlen,
                        "surface node-row domains differ: '%.350s' has %d rows but '%.350s' has %d",
                        DSET_HEADNAME(aa),na,DSET_HEADNAME(bb),nb) ;
     return 0 ;
   }
   sa=(aa->dblk->node_list!=NULL) ; sb=(bb->dblk->node_list!=NULL) ;
   if( (sa && aa->dblk->nnodes!=na) || (sb && bb->dblk->nnodes!=nb) ){
     thd_patterns_error(err,errlen,
                        "surface node-row metadata is malformed in '%.430s' or '%.430s'",
                        DSET_HEADNAME(aa),DSET_HEADNAME(bb)) ;
     return 0 ;
   }
   for( ii=0 ; ii<na ; ii++ ){
     int an=sa?aa->dblk->node_list[ii]:ii ;
     int bn=sb?bb->dblk->node_list[ii]:ii ;
     if( an<0 || bn<0 ){
       thd_patterns_error(err,errlen,
                          "surface node-row metadata is malformed at row %d: node IDs must be nonnegative",
                          ii+1) ;
       return 0 ;
     }
     if( an!=bn ){
       thd_patterns_error(err,errlen,
                          "surface node-row domains differ at row %d: '%.350s' has node %d, '%.350s' has node %d",
                          ii+1,DSET_HEADNAME(aa),an,DSET_HEADNAME(bb),bn) ;
       return 0 ;
     }
     if( ii>0 ){
       int prev=sa?aa->dblk->node_list[ii-1]:ii-1 ;
       if( an==prev ){
         thd_patterns_error(err,errlen,"surface node-row metadata repeats node %d",an) ;
         return 0 ;
       }
       if( an<prev ) ordered=0 ;
     }
   }
   /* Compare rows in their original order, but check uniqueness on a copy.
      Node IDs can be large and need not be sorted or contiguous. */
   if( (sa || sb) && !ordered ){
     int *nodes=(int *)malloc(sizeof(int)*(size_t)na) ;
     if( nodes==NULL ){
       thd_patterns_error(err,errlen,"out of memory validating surface node IDs") ;
       return 0 ;
     }
     memcpy(nodes,sa?aa->dblk->node_list:bb->dblk->node_list,sizeof(int)*(size_t)na) ;
     qsort(nodes,(size_t)na,sizeof(int),thd_node_id_cmp) ;
     for( ii=1 ; ii<na ; ii++ ) if( nodes[ii]==nodes[ii-1] ){
       thd_patterns_error(err,errlen,"surface node-row metadata repeats node %d",nodes[ii]) ;
       free(nodes) ; return 0 ;
     }
     free(nodes) ;
   }
   return 1 ;
}

int THD_copy_surface_node_domain( THD_3dim_dataset *dst,
                                  const THD_3dim_dataset *src,
                                  char *err, size_t errlen )
{
   int nn ; int *copy=NULL ;
   if( err!=NULL && errlen>0 ) err[0]='\0' ;
   if( dst==NULL || src==NULL || !ISVALID_DBLK(dst->dblk) || !ISVALID_DBLK(src->dblk) ){
     thd_patterns_error(err,errlen,"cannot copy a surface node domain from/to a null dataset") ;
     return 0 ;
   }
   if( !thd_patterns_is_surface(src) ) return 1 ;
   if( !THD_surface_domain_match(src,src,err,errlen) ) return 0 ;
   if( !thd_patterns_is_surface(dst) ){
     thd_patterns_error(err,errlen,
                        "cannot copy a surface node domain into a non-surface dataset") ;
     return 0 ;
   }
   if( DSET_NVOX(dst)!=DSET_NVOX(src) ){
     thd_patterns_error(err,errlen,
                        "cannot copy a %d-row surface node domain into a %d-row dataset",
                        DSET_NVOX(src),DSET_NVOX(dst)) ;
     return 0 ;
   }
   if( src->dblk->node_list==NULL ){
     if( dst!=src && dst->dblk->node_list!=NULL ) myRwcFree(dst->dblk->node_list) ;
     if( dst!=src ) dst->dblk->nnodes=0 ;
     return 1 ;
   }
   nn=src->dblk->nnodes ;
   if( nn!=DSET_NVOX(src) ){
     thd_patterns_error(err,errlen,
                        "surface node-row metadata is malformed in '%.430s': %d rows but %d node IDs",
                        DSET_HEADNAME(src),DSET_NVOX(src),nn) ;
     return 0 ;
   }
   if( dst==src ) return 1 ;
   copy=(int *)RwcMalloc(sizeof(int)*(size_t)nn) ;
   if( copy==NULL ){
     thd_patterns_error(err,errlen,"out of memory copying %d surface node IDs",nn) ;
     return 0 ;
   }
   memcpy(copy,src->dblk->node_list,sizeof(int)*(size_t)nn) ;
   if( dst->dblk->node_list!=NULL ) myRwcFree(dst->dblk->node_list) ;
   dst->dblk->node_list=copy ; dst->dblk->nnodes=nn ;
   return 1 ;
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
   SUMA_SurfaceObject *SO=NULL ;
   SUMA_GET_OFFSET_STRUCT *OffS=NULL ;
   THD_roilist *rl=NULL ;
   MRI_IMAGE *mim=NULL ; float *mar , fac ;
   byte *mask=NULL ; int *n2i=NULL , *tmp=NULL ;
   int nvox=0 , nnode , sparse , ncen=0 , m , kk , ii ;

   if( err != NULL && errlen > 0 ) err[0]='\0' ;
   if( surf_file==NULL || surf_file[0]=='\0' || mset==NULL ||
       !isfinite(radius) || radius<=0.0f ){
     thd_patterns_error(err,errlen,"surface searchlight needs a surface file, dataset, and finite radius > 0") ;
     return NULL ;
   }
   nvox=DSET_NVOX(mset) ;
   if( !(DBLK_IS_NI_SURF_DSET(mset->dblk) || DBLK_IS_GIFTI(mset->dblk)) ){
     thd_patterns_error(err,errlen,"surface searchlight dataset '%s' is not a NIML/GIFTI surface dataset",
                        DSET_HEADNAME(mset)) ;
     return NULL ;
   }
   DSET_load(mset) ;
   if( !DSET_LOADED(mset) ){
     thd_patterns_error(err,errlen,"cannot load surface searchlight dataset '%s'",DSET_HEADNAME(mset)) ;
     return NULL ;
   }
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
     SUMA_Free_Surface_Object(SO) ; return NULL ;
   }
   nnode = SO->N_Node ;
   if( nnode<1 ){
     thd_patterns_error(err,errlen,"surface '%s' has no nodes",surf_file) ;
     SUMA_Free_Surface_Object(SO) ; return NULL ;
   }

   sparse = (mset->dblk->node_list != NULL) ;
   if( sparse && mset->dblk->nnodes!=nvox ){
     thd_patterns_error(err,errlen,
                        "surface dataset '%s' has %d rows but %d node indexes",
                        DSET_HEADNAME(mset),nvox,mset->dblk->nnodes) ;
     SUMA_Free_Surface_Object(SO) ; return NULL ;
   }
   if( !sparse && nvox!=nnode ){
     thd_patterns_error(err,errlen,
                        "dense surface dataset '%s' has %d rows but mesh '%s' has %d nodes",
                        DSET_HEADNAME(mset),nvox,surf_file,nnode) ;
     SUMA_Free_Surface_Object(SO) ; return NULL ;
   }
   n2i = (int *)malloc(sizeof(int)*nnode) ;
   if( n2i==NULL ){
     thd_patterns_error(err,errlen,"out of memory validating the surface node domain") ;
     SUMA_Free_Surface_Object(SO) ; return NULL ;
   }
   for( m=0 ; m<nnode ; m++ ) n2i[m]=-1 ;
   if( sparse ){
     for( ii=0 ; ii<mset->dblk->nnodes ; ii++ ){
       int nid=mset->dblk->node_list[ii] ;
       if( nid<0 || nid>=nnode ){
         thd_patterns_error(err,errlen,
                            "surface dataset '%s' row %d has node %d outside mesh range 0..%d",
                            DSET_HEADNAME(mset),ii+1,nid,nnode-1) ;
         free(n2i) ; SUMA_Free_Surface_Object(SO) ; return NULL ;
       }
       if( n2i[nid]>=0 ){
         thd_patterns_error(err,errlen,
                            "surface dataset '%s' repeats mesh node %d in rows %d and %d",
                            DSET_HEADNAME(mset),nid,n2i[nid]+1,ii+1) ;
         free(n2i) ; SUMA_Free_Surface_Object(SO) ; return NULL ;
       }
       n2i[nid]=ii ;
     }
   } else {
     for( ii=0 ; ii<nnode ; ii++ ) n2i[ii]=ii ;
   }

   mask=(byte *)calloc(nvox,sizeof(byte)) ;
   if( mask==NULL ){
     thd_patterns_error(err,errlen,"out of memory reading the surface mask") ;
     free(n2i) ; SUMA_Free_Surface_Object(SO) ; return NULL ;
   }
   if( all_nodes ){
     for( ii=0 ; ii<nvox ; ii++ ) mask[ii]=1 ;
   } else {
     fac=DSET_BRICK_FACTOR(mset,0) ; if( fac==0.0f ) fac=1.0f ;
     mim=mri_to_float(DSET_BRICK(mset,0)) ;
     if( mim==NULL ){
       thd_patterns_error(err,errlen,"cannot convert surface mask '%s' to float",
                          DSET_HEADNAME(mset)) ;
       free(n2i) ; free(mask) ; SUMA_Free_Surface_Object(SO) ; return NULL ;
     }
     mar=MRI_FLOAT_PTR(mim) ;
     for( ii=0 ; ii<nvox ; ii++ ){
       float vv=mar[ii]*fac ;
       if( !isfinite(vv) ){
         int nid=sparse?mset->dblk->node_list[ii]:ii ;
         thd_patterns_error(err,errlen,
                            "surface mask '%s' is not finite at mesh node %d",
                            DSET_HEADNAME(mset),nid) ;
         mri_free(mim) ; free(n2i) ; free(mask) ; SUMA_Free_Surface_Object(SO) ;
         return NULL ;
       }
       if( vv != 0.0f ) mask[ii]=1 ;
     }
     mri_free(mim) ;
   }

   for( m=0 ; m<nnode ; m++ ) if( n2i[m]>=0 && mask[n2i[m]] ) ncen++ ;
   if( ncen==0 ){
     free(n2i) ; free(mask) ; SUMA_Free_Surface_Object(SO) ; return NULL ;
   }

   rl=(THD_roilist *)calloc(1,sizeof(THD_roilist)) ;
   if( rl==NULL ){
     thd_patterns_error(err,errlen,"out of memory allocating surface searchlights") ;
     free(n2i) ; free(mask) ; SUMA_Free_Surface_Object(SO) ; return NULL ;
   }
   rl->nroi=ncen ;
   rl->val=(int *)malloc(sizeof(int)*ncen) ;
   rl->vox=(intvec *)calloc(ncen,sizeof(intvec)) ;
   rl->lab=(char **)calloc(ncen,sizeof(char *)) ;
   rl->center=(int *)malloc(sizeof(int)*ncen) ;
   if( rl->val==NULL || rl->vox==NULL || rl->lab==NULL || rl->center==NULL ){
     thd_patterns_error(err,errlen,"out of memory allocating surface searchlight indexes") ;
     THD_roilist_free(rl) ; free(n2i) ; free(mask) ; SUMA_Free_Surface_Object(SO) ;
     return NULL ;
   }

   OffS=SUMA_Initialize_getoffsets(nnode) ;
   tmp=(int *)malloc(sizeof(int)*nnode) ;
   if( OffS==NULL || tmp==NULL ){
     thd_patterns_error(err,errlen,"out of memory allocating geodesic searchlight workspace") ;
     if( OffS!=NULL ) SUMA_Free_getoffsets(OffS) ;
     free(tmp) ; THD_roilist_free(rl) ; free(n2i) ; free(mask) ;
     SUMA_Free_Surface_Object(SO) ; return NULL ;
   }

   for( kk=0,m=0 ; m<nnode ; m++ ){
     int ci=(n2i[m]>=0) ? n2i[m] : -1 , il,jl,n ;
     if( ci<0 || !mask[ci] ) continue ;

     if( !SUMA_getoffsets2(m,SO,radius,OffS,NULL,0) ){
       thd_patterns_error(err,errlen,"failed finding geodesic neighbors of mesh node %d",m) ;
       SUMA_Free_getoffsets(OffS) ; free(tmp) ; THD_roilist_free(rl) ;
       free(n2i) ; free(mask) ; SUMA_Free_Surface_Object(SO) ; return NULL ;
     }
     n=0 ; tmp[n++]=ci ;
     for( il=1 ; il<OffS->N_layers ; il++ )
       for( jl=0 ; jl<OffS->layers[il].N_NodesInLayer ; jl++ ){
         int nb=OffS->layers[il].NodesInLayer[jl] ;
         if( OffS->OffVect[nb]<=radius && n2i[nb]>=0 && mask[n2i[nb]] )
           tmp[n++]=n2i[nb] ;
       }
     SUMA_Recycle_getoffsets(OffS) ;

     rl->vox[kk].ar=(int *)malloc(sizeof(int)*n) ;
     if( rl->vox[kk].ar==NULL ){
       thd_patterns_error(err,errlen,"out of memory storing searchlight at mesh node %d",m) ;
       SUMA_Free_getoffsets(OffS) ; free(tmp) ; THD_roilist_free(rl) ;
       free(n2i) ; free(mask) ; SUMA_Free_Surface_Object(SO) ; return NULL ;
     }
     memcpy(rl->vox[kk].ar,tmp,sizeof(int)*n) ;
     rl->vox[kk].nar=n ;
     rl->val[kk]=m ;             /* reported mesh node ID */
     rl->center[kk]=ci ;         /* row painted in the sparse/dense dataset */
     kk++ ;
   }

   SUMA_Free_getoffsets(OffS) ;
   free(tmp) ; free(n2i) ; free(mask) ;
   SUMA_Free_Surface_Object(SO) ;
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

static void THD_roi_mean_ts_internal( THD_3dim_dataset *dset , intvec *vox ,
                                      int polort , float *out ,
                                      unsigned char *keep )
{
   int nvals = DSET_NVALS(dset) , ii , tt ;

   for( tt=0 ; tt < nvals ; tt++ ){
     out[tt] = 0.0f ; if( keep != NULL ) keep[tt] = 0 ;
   }
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
          if( fac==1.0f ) for( ii=0 ; ii<vox->nar ; ii++ ){                    \
            float vv=(float)(expr) ; out[tt]+=vv ;                              \
            if( keep != NULL && vv != 0.0f ) keep[tt]=1 ;                       \
          } else for( ii=0 ; ii<vox->nar ; ii++ ){                             \
            float vv=(float)(expr)*fac ; out[tt]+=vv ;                          \
            if( keep != NULL && vv != 0.0f ) keep[tt]=1 ;                       \
          }                                                                      \
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

void THD_roi_mean_ts( THD_3dim_dataset *dset , intvec *vox ,
                      int polort , float *out )
{
   THD_roi_mean_ts_internal(dset,vox,polort,out,NULL) ;
}

void THD_roi_mean_ts_zmask( THD_3dim_dataset *dset , intvec *vox ,
                            int polort , float *out , unsigned char *keep )
{
   THD_roi_mean_ts_internal(dset,vox,polort,out,keep) ;
}

static int thd_roi_brick_pattern( THD_3dim_dataset *dset, int bb,
                                  const intvec *vox, float *out )
{
   int nvx=vox->nar , ii ;
   void *ar ; float fac ;

   for( ii=0 ; ii<nvx ; ii++ ) out[ii]=0.0f ;
   if( dset==NULL || bb<0 || bb>=DSET_NVALS(dset) ) return 0 ;
   ar=DSET_ARRAY(dset,bb) ;
   if( ar==NULL ){ DSET_load(dset) ; ar=DSET_ARRAY(dset,bb) ; }
   if( ar==NULL ) return 0 ;
   fac=DSET_BRICK_FACTOR(dset,bb) ;
   if( fac<=0.0f ) fac=1.0f ;             /* match THD_get_voxel exactly */

#define COPY_SCALAR_PATTERN(type)                                                \
   do { type *src=(type *)ar ;                                                   \
        if( fac==1.0f )                                                          \
          for( ii=0 ; ii<nvx ; ii++ ) out[ii]=(float)src[vox->ar[ii]] ;          \
        else                                                                     \
          for( ii=0 ; ii<nvx ; ii++ )                                            \
            out[ii]=(float)src[vox->ar[ii]]*fac ;                                \
   } while(0)

   switch( DSET_BRICK_TYPE(dset,bb) ){
     default: return 0 ;
     case MRI_byte:   COPY_SCALAR_PATTERN(byte)   ; break ;
     case MRI_short:  COPY_SCALAR_PATTERN(short)  ; break ;
     case MRI_int:    COPY_SCALAR_PATTERN(int)    ; break ;
     case MRI_float:  COPY_SCALAR_PATTERN(float)  ; break ;
     case MRI_double: COPY_SCALAR_PATTERN(double) ; break ;

     case MRI_complex:{
       complex *src=(complex *)ar ;
       for( ii=0 ; ii<nvx ; ii++ ){
         complex c=src[vox->ar[ii]] ;
         out[ii]=sqrtf(c.r*c.r+c.i*c.i)*fac ;
       }
     }
     break ;

     case MRI_rgb:{
       rgbyte *src=(rgbyte *)ar ;
       for( ii=0 ; ii<nvx ; ii++ ){
         rgbyte c=src[vox->ar[ii]] ;
         out[ii]=(0.299f*(float)c.r + 0.587f*(float)c.g
                                + 0.114f*(float)c.b)*fac ;
       }
     }
     break ;

     case MRI_rgba:{
       rgba *src=(rgba *)ar ;
       for( ii=0 ; ii<nvx ; ii++ ){
         rgba c=src[vox->ar[ii]] ;
         float val=0.299f*(float)c.r + 0.587f*(float)c.g
                                      + 0.114f*(float)c.b ;
         out[ii]=val*(0.00392157f*(float)c.a)*fac ;
       }
     }
     break ;
   }
#undef COPY_SCALAR_PATTERN
   return 1 ;
}

void THD_roi_pattern( THD_3dim_dataset *dset , intvec *vox , float *out )
{
   int nvals = DSET_NVALS(dset) , nvx = vox->nar , bb ;

   /* THD_get_voxel performs dataset/type/bounds dispatch for every value.
      These voxel indices have already been validated while the ROI list was
      built, so dispatch once per brick and walk its typed array directly. */
   for( bb=0 ; bb<nvals ; bb++ )
     (void)thd_roi_brick_pattern(dset,bb,vox,out+(size_t)bb*nvx) ;
}

int THD_roi_samples( const THD_sampleset *ss,
                     const int *sample_index, int nsample,
                     const intvec *vox, float *out,
                     char *err, size_t errlen )
{
   int ii,jj,rr,dd,bb,nvx ; THD_3dim_dataset *dset ; float *dst ;

   if( err!=NULL && errlen>0 ) err[0]='\0' ;
   if( ss==NULL || ss->dset==NULL || ss->sample_dset==NULL ||
       ss->sample_brick==NULL || ss->nsample<1 || ss->ndset<1 || ss->nvox<1 ){
     thd_patterns_error(err,errlen,"invalid or empty sample set") ; return 0 ;
   }
   if( vox==NULL || vox->ar==NULL || vox->nar<1 || out==NULL || nsample<1 ){
     thd_patterns_error(err,errlen,"sample extraction needs samples, voxels, and output storage") ;
     return 0 ;
   }
   if( sample_index==NULL && nsample!=ss->nsample ){
     thd_patterns_error(err,errlen,
       "NULL sample index selects all %d canonical rows, not %d",ss->nsample,nsample) ;
     return 0 ;
   }
   nvx=vox->nar ;
   if( (size_t)nsample>SIZE_MAX/sizeof(float)/(size_t)nvx ){
     thd_patterns_error(err,errlen,"requested sample-by-voxel matrix is too large") ; return 0 ;
   }
   for( jj=0 ; jj<nvx ; jj++ ) if( vox->ar[jj]<0 || vox->ar[jj]>=ss->nvox ){
     thd_patterns_error(err,errlen,
       "ROI voxel %d has index %d outside [0,%d]",jj+1,vox->ar[jj],ss->nvox-1) ;
     return 0 ;
   }

   for( ii=0 ; ii<nsample ; ii++ ){
     rr=(sample_index==NULL) ? ii : sample_index[ii] ;
     if( rr<0 || rr>=ss->nsample ){
       thd_patterns_error(err,errlen,
         "requested sample %d has canonical index %d outside [0,%d]",
         ii+1,rr,ss->nsample-1) ;
       return 0 ;
     }
     dd=ss->sample_dset[rr] ; bb=ss->sample_brick[rr] ;
     if( dd<0 || dd>=ss->ndset || (dset=ss->dset[dd])==NULL ){
       thd_patterns_error(err,errlen,
         "canonical sample %d refers to invalid dataset index %d",rr+1,dd) ;
       return 0 ;
     }
     if( DSET_NVOX(dset)!=ss->nvox ){
       thd_patterns_error(err,errlen,
         "canonical sample %d dataset has %d voxels; expected %d",
         rr+1,DSET_NVOX(dset),ss->nvox) ;
       return 0 ;
     }
     if( bb<0 || bb>=DSET_NVALS(dset) ){
       thd_patterns_error(err,errlen,
         "canonical sample %d refers to invalid brick index %d",rr+1,bb) ;
       return 0 ;
     }
     dst=out+(size_t)ii*nvx ;
     if( !thd_roi_brick_pattern(dset,bb,vox,dst) ){
       const char *name=(ss->dset_name!=NULL && ss->dset_name[dd]!=NULL)
                        ? ss->dset_name[dd] : "unnamed dataset" ;
       thd_patterns_error(err,errlen,
         "cannot extract canonical sample %d from %s brick %d",rr+1,name,bb) ;
       return 0 ;
     }
     for( jj=0 ; jj<nvx ; jj++ ) if( !isfinite(dst[jj]) ){
       const char *label=(ss->sample_label!=NULL && ss->sample_label[rr]!=NULL)
                         ? ss->sample_label[rr] : "unlabeled" ;
       thd_patterns_error(err,errlen,
         "canonical sample %d (%s), voxel index %d is not finite",
         rr+1,label,vox->ar[jj]) ;
       return 0 ;
     }
   }
   return 1 ;
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

/*! Split an internal comma-separated Xmat condition list. */
static char ** read_inline_conditions( const char *text, int want )
{
   char **lab,*copy,*tok ; int n=0,ii ;
   if( text==NULL || want<1 ) return NULL ;
   lab=(char **)calloc(want,sizeof(char *)) ; copy=strdup(text) ;
   if( lab==NULL || copy==NULL ){ free(lab) ; free(copy) ; return NULL ; }
   for( tok=strtok(copy,",") ; tok!=NULL ; tok=strtok(NULL,",") ){
     if( n>=want || *tok=='\0' ) goto bad ;
     lab[n++]=strdup(tok) ;
   }
   free(copy) ;
   copy=NULL ;
   if( n==want ) return lab ;
bad:
   free(copy) ; for( ii=0 ; ii<want ; ii++ ) free(lab[ii]) ; free(lab) ;
   return NULL ;
}

/*! Return a whole-Xmat run label as a zero-based run index. */
static int xmat_run_index( const char *text, int nrun )
{
   char *end ; long v ;
   if( text==NULL || *text=='\0' ) return -1 ;
   errno=0 ; v=strtol(text,&end,10) ;
   if( errno || *end!='\0' || v<1 || v>nrun ) return -1 ;
   return (int)v-1 ;
}

/*! Add the Xmat-defined uncensored samples of run rr as an AFNI selector. */
static char * xmat_resid_selector( const char *base, const THD_xmat *xm, int rr,
                                   const char *subj )
{
   THD_3dim_dataset *ds ; char *out ; size_t cap,pos ; int ii,first=-1,last=-1,n=0 ;
   int lo,hi ;
   if( base==NULL || xm==NULL || rr<0 || rr>=xm->nrun ) return NULL ;
   if( strchr(base,'[')!=NULL ){
     ERROR_message("Subj %s: with Xmat, ResidFile must be the whole residual dataset,\n"
                   "       without a selector ('%s')",subj,base) ; return NULL ;
   }
   ds=THD_open_dataset((char *)base) ;
   if( ds==NULL ){
     ERROR_message("Subj %s: cannot open whole ResidFile '%s'",subj,base) ; return NULL ;
   }
   if( DSET_NVALS(ds)!=xm->nfull ){
     ERROR_message("Subj %s: ResidFile '%s' has %d volumes but Xmat '%s' describes\n"
                   "       %d full-timeline volumes",subj,base,DSET_NVALS(ds),
                   xm->fname,xm->nfull) ; DSET_delete(ds) ; return NULL ;
   }
   DSET_delete(ds) ;
   lo=xm->runstart[rr] ; hi=lo+THD_xmat_run_length(xm,rr) ;
   for( ii=0 ; ii<xm->nrow ; ii++ ) if( xm->good[ii]>=lo && xm->good[ii]<hi ) n++ ;
   if( n<1 ){
     ERROR_message("Subj %s: Xmat '%s' run %d has no uncensored residual samples",
                   subj,xm->fname,rr+1) ; return NULL ;
   }
   cap=strlen(base)+(size_t)n*24+4 ; out=(char *)malloc(cap) ;
   if( out==NULL ) return NULL ;
   pos=(size_t)snprintf(out,cap,"%s[",base) ;
   for( ii=0 ; ii<xm->nrow ; ii++ ){
     int t=xm->good[ii] ;
     if( t<lo || t>=hi ) continue ;
     if( first<0 ){ first=last=t ; continue ; }
     if( t==last+1 ){ last=t ; continue ; }
     if( pos>strlen(base)+1 ) out[pos++]=',' ;
     if( last>first ) pos+=(size_t)snprintf(out+pos,cap-pos,"%d..%d",first,last) ;
     else             pos+=(size_t)snprintf(out+pos,cap-pos,"%d",first) ;
     first=last=t ;
   }
   if( first>=0 ){
     if( pos>strlen(base)+1 ) out[pos++]=',' ;
     if( last>first ) pos+=(size_t)snprintf(out+pos,cap-pos,"%d..%d",first,last) ;
     else             pos+=(size_t)snprintf(out+pos,cap-pos,"%d",first) ;
   }
   snprintf(out+pos,cap-pos,"]") ; return out ;
}

/*! Replace each whole ResidFile in an Xmat-bearing run table by the valid
    samples of that row's one-based numeric Run. */
static int runset_apply_xmat_residuals( THD_datatable *dt, int iRun, int iXmat,
                                        int iResid )
{
   int ii ; char err[512] ;
   if( iXmat<0 ) return 1 ;
   for( ii=0 ; ii<dt->nrow ; ii++ ){
     THD_xmat *xm=THD_xmat_read(DT_CELL(dt,ii,iXmat),err,sizeof(err)) ; int rr ;
     if( xm==NULL ){
       ERROR_message("Subj %s: %s",dt->subj[ii],err) ; return 0 ;
     }
     rr=xmat_run_index(DT_CELL(dt,ii,iRun),xm->nrun) ;
     if( rr<0 ){
       ERROR_message("Subj %s: Run '%s' must be a one-based integer in 1..%d\n"
                     "       when an Xmat column supplies run boundaries",
                     dt->subj[ii],DT_CELL(dt,ii,iRun),xm->nrun) ;
       THD_xmat_free(xm) ; return 0 ;
     }
     if( iResid>=0 ){
       char *sel=xmat_resid_selector(DT_CELL(dt,ii,iResid),xm,rr,dt->subj[ii]) ;
       if( sel==NULL ){ THD_xmat_free(xm) ; return 0 ; }
       free(DT_CELL(dt,ii,iResid)) ; DT_CELL(dt,ii,iResid)=sel ;
     }
     THD_xmat_free(xm) ;
   }
   return 1 ;
}

static THD_runset * runset_read_table( THD_datatable *dt, const char *fname )
{
   THD_runset *rs ;
   int iRun , iResid , iCond , iTrial , iInline , iOrder , iXmat ;
   int i , s , r , r2 , ok=1 ;
   char domain_err[1024] ;

   if( dt == NULL || fname == NULL ) return NULL ;

   if( dt->icol_subj < 0 || dt->icol_input < 0 ){
     ERROR_message("runwiseTable '%s' needs Subj and InputFile columns",fname) ;
     THD_free_datatable(dt) ; return NULL ;
   }
   iRun   = THD_datatable_column( dt , "Run" ) ;
   iResid = THD_datatable_column( dt , "ResidFile" ) ;
   iCond  = THD_datatable_column( dt , "ConditionFile" ) ;
   iTrial = THD_datatable_column( dt , "TrialFile" ) ;
   iInline= THD_datatable_column( dt , "_XmatConditions" ) ;
   iOrder = THD_datatable_column( dt , "_XmatConditionOrder" ) ;
   iXmat  = THD_datatable_column( dt , "Xmat" ) ;
   if( iRun < 0 ){
     ERROR_message("runwiseTable '%s' needs a 'Run' column (one row per subject x run)",fname) ;
     THD_free_datatable(dt) ; return NULL ;
   }
   if( iCond>=0 && iTrial>=0 ){
     ERROR_message("runwiseTable '%s' cannot contain both ConditionFile and TrialFile;\n"
                   "       TrialFile already supplies each trial's condition",fname) ;
     THD_free_datatable(dt) ; return NULL ;
   }
   if( iInline>=0 && (iCond>=0 || iTrial>=0) ){
     ERROR_message("runwiseTable '%s' has conflicting internal and explicit condition mappings",fname) ;
     THD_free_datatable(dt) ; return NULL ;
   }
   if( iOrder>=0 && iInline<0 ){
     ERROR_message("runwiseTable '%s' has an internal Xmat order without Xmat conditions",fname) ;
     THD_free_datatable(dt) ; return NULL ;
   }
   if( !runset_apply_xmat_residuals(dt,iRun,iXmat,iResid) ){
     THD_free_datatable(dt) ; return NULL ;
   }

   rs = (THD_runset *)calloc(1,sizeof(THD_runset)) ;
   rs->nrow      = dt->nrow ;
   rs->has_resid = (iResid >= 0) ;
   rs->has_condmap = (iCond >= 0 || iTrial >= 0 || iInline >= 0) ;
   rs->has_trialmap= (iTrial >= 0) ;
   rs->from_xmat  = (iInline >= 0) ;
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

   /* Compact Xmat expansion carries the selected StimLabels order explicitly,
      so a condition absent from the first run cannot change model/RDM order. */
   if( iOrder>=0 ){
     const char *ord=DT_CELL(dt,0,iOrder) ; int nc=1 ;
     const char *pp ;
     for( pp=ord ; *pp ; pp++ ) if( *pp==',' ) nc++ ;
     for( i=1 ; i<dt->nrow ; i++ ) if( strcmp(ord,DT_CELL(dt,i,iOrder)) ){
       ERROR_message("xmatTable: subjects have different selected StimLabels order") ;
       ok=0 ; break ;
     }
     if( ok ){
       rs->cond_lab=read_inline_conditions(ord,nc) ;
       if( rs->cond_lab==NULL ) ok=0 ; else rs->ncond=nc ;
     }
   }
   if( !ok ){
     THD_free_datatable(dt) ; THD_runset_free(rs) ; return NULL ;
   }

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
   if( iOrder<0 ) rs->ncond = rs->has_condmap ? 0 : -1 ;
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
     else if( DSET_NVOX(ds) != rs->nvox ){
       ERROR_message("runwiseTable: '%s' has %d rows/voxels; the first row has %d",
                     dt->fname[i],DSET_NVOX(ds),rs->nvox) ; ok=0 ; break ;
     } else if( thd_patterns_is_surface(ds) || thd_patterns_is_surface(rs->betas[0]) ){
       if( !THD_surface_domain_match(ds,rs->betas[0],domain_err,sizeof(domain_err)) ){
         ERROR_message("runwiseTable: '%s': %s",dt->fname[i],domain_err) ; ok=0 ; break ;
       }
     } else if( !EQUIV_GRIDS(ds,rs->betas[0]) ){
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
       } else if( iCond>=0 ){
         lab=read_condition_file(DT_CELL(dt,i,iCond),rs->nbrick[i]) ;
       } else {
         lab=read_inline_conditions(DT_CELL(dt,i,iInline),rs->nbrick[i]) ;
         if( lab==NULL )
           ERROR_message("Subj %s run %s: Xmat condition count does not match its %d beta bricks",
                         dt->subj[i],DT_CELL(dt,i,iRun),rs->nbrick[i]) ;
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
       if( DSET_NVOX(rd) != rs->nvox ){
         ERROR_message("runwiseTable: ResidFile '%s' has %d rows/voxels; InputFile has %d",
                       rf,DSET_NVOX(rd),rs->nvox) ;
         DSET_delete(rd) ; ok=0 ; break ;
       }
       if( thd_patterns_is_surface(rd) || thd_patterns_is_surface(rs->betas[0]) ){
         if( !THD_surface_domain_match(rd,rs->betas[0],domain_err,sizeof(domain_err)) ){
           ERROR_message("runwiseTable: ResidFile '%s': %s",rf,domain_err) ;
           DSET_delete(rd) ; ok=0 ; break ;
         }
       } else if( !EQUIV_GRIDS(rd,rs->betas[0]) ){
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
     } else if( rs->from_xmat ){
       rs->nrep=(int **)calloc(rs->nrow,sizeof(int *)) ;
       if( rs->nrep==NULL ) ok=0 ;
       for( i=0 ; i<rs->nrow && ok ; i++ ){
         rs->nrep[i]=(int *)calloc(rs->ncond,sizeof(int)) ;
         if( rs->nrep[i]==NULL ){ ok=0 ; break ; }
         for( bb=0 ; bb<rs->nbrick[i] ; bb++ ) rs->nrep[i][rs->cond_of[i][bb]]++ ;
       }
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

   /* A compact Xmat table often expands to the original balanced contract:
      one beta for every condition, in StimLabels order, in every run.  Keep
      that representation on the fast/common path (and preserve metrics whose
      covariance assumes a fixed complete condition set). */
   if( ok && rs->from_xmat && rs->has_condmap ){
     int balanced=1,bb ;
     for( i=0 ; i<rs->nrow && balanced ; i++ ){
       if( rs->nbrick[i]!=rs->ncond ) balanced=0 ;
       for( bb=0 ; bb<rs->nbrick[i] && balanced ; bb++ )
         if( rs->cond_of[i][bb]!=bb || rs->nrep[i][bb]!=1 ) balanced=0 ;
     }
     if( balanced ){
       for( i=0 ; i<rs->nrow ; i++ ){ free(rs->cond_of[i]) ; free(rs->nrep[i]) ; }
       free(rs->cond_of) ; free(rs->nrep) ; rs->cond_of=NULL ; rs->nrep=NULL ;
       rs->has_condmap=0 ;
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

THD_runset * THD_runset_read( char *fname )
{
   THD_datatable *dt ;
   if( fname==NULL ) return NULL ;
   dt=THD_read_datatable_file(fname) ;
   if( dt==NULL ){
     ERROR_message("runwiseTable: cannot read '%s'",fname) ; return NULL ;
   }
   return runset_read_table(dt,fname) ;
}

typedef struct { int n,cap ; char **v ; } runset_tokens ;

static int runset_tok_add( runset_tokens *t, const char *s )
{
   if( t->n==t->cap ){
     int nc=(t->cap>0)?2*t->cap:64 ;
     char **nv=(char **)realloc(t->v,sizeof(char *)*(size_t)nc) ;
     if( nv==NULL ) return 0 ; t->v=nv ; t->cap=nc ;
   }
   t->v[t->n]=strdup(s?s:"") ;
   if( t->v[t->n]==NULL ) return 0 ; t->n++ ; return 1 ;
}

static void runset_tok_free( runset_tokens *t )
{
   int ii ; if( t==NULL ) return ;
   for( ii=0 ; ii<t->n ; ii++ ) free(t->v[ii]) ;
   free(t->v) ; t->v=NULL ; t->n=t->cap=0 ;
}

THD_runset * THD_runset_read_xmat( char *fname, char *stims )
{
   THD_datatable *src=NULL,*expanded=NULL ; THD_runset *out=NULL ;
   runset_tokens t={0,0,NULL} ; int ix,ir,ii,jj,rr,has_resid,ok=1,nrequested=0 ;
   char err[512] ;
   if( fname==NULL ) return NULL ;
   src=THD_read_datatable_file(fname) ;
   if( src==NULL ) return NULL ;
   ix=THD_datatable_column(src,"Xmat") ; ir=THD_datatable_column(src,"ResidFile") ;
   if( src->icol_subj<0 || src->icol_input<0 || ix<0 ){
     ERROR_message("xmatTable '%s' needs Subj, Xmat and InputFile columns",fname) ;
     ok=0 ; goto done ;
   }
   if( THD_datatable_column(src,"Run")>=0 ){
     ERROR_message("xmatTable '%s' is one row per subject; remove its Run column",fname) ;
     ok=0 ; goto done ;
   }
   for( ii=0 ; ii<src->ncol ; ii++ ){
     const char *c=src->cname[ii] ;
     if( strcasecmp(c,"Subj") && strcasecmp(c,"Xmat") &&
         strcasecmp(c,"InputFile") && strcasecmp(c,"ResidFile") ){
       ERROR_message("xmatTable '%s': unsupported column '%s'; the compact table uses\n"
                     "       only Subj, Xmat, InputFile and optional ResidFile",fname,c) ;
       ok=0 ; goto done ;
     }
   }
   for( ii=0 ; ii<src->nrow ; ii++ ) for( jj=ii+1 ; jj<src->nrow ; jj++ )
     if( !strcmp(src->subj[ii],src->subj[jj]) ){
       ERROR_message("xmatTable '%s' has two rows for Subj %s",fname,src->subj[ii]) ;
       ok=0 ; goto done ;
     }

   has_resid=(ir>=0) ;
   if( stims!=NULL && *stims!='\0' ){
     const char *pp ; nrequested=1 ;
     for( pp=stims ; *pp ; pp++ ) if( *pp==',' ) nrequested++ ;
   }
   ok &= runset_tok_add(&t,"Subj") ; ok &= runset_tok_add(&t,"Run") ;
   ok &= runset_tok_add(&t,"Xmat") ;
   if( has_resid ) ok &= runset_tok_add(&t,"ResidFile") ;
   ok &= runset_tok_add(&t,"_XmatConditions") ;
   ok &= runset_tok_add(&t,"_XmatConditionOrder") ;
   ok &= runset_tok_add(&t,"InputFile") ;
   if( !ok ) goto done ;

   for( ii=0 ; ii<src->nrow && ok ; ii++ ){
     const char *bucket=DT_CELL(src,ii,src->icol_input) ;
     THD_xmat *xm=NULL ; THD_xmat_item *it=NULL ; THD_3dim_dataset *ds=NULL ;
     unsigned char *want=NULL ; int nitem=0,nfound=0,*brick=NULL ; char *order=NULL ;
     if( strchr(bucket,'[')!=NULL ){
       ERROR_message("Subj %s: xmatTable InputFile must be the whole stats bucket,\n"
                     "       without a selector ('%s')",src->subj[ii],bucket) ; ok=0 ;
       continue ;
     }
     xm=THD_xmat_read(DT_CELL(src,ii,ix),err,sizeof(err)) ;
     if( xm==NULL ){
       ERROR_message("Subj %s: %s",src->subj[ii],err) ; ok=0 ; continue ;
     }
     want=THD_xmat_stims_wanted(xm,stims,&nfound) ;
     if( nfound<1 || (nrequested>0 && nfound!=nrequested) ){
       ERROR_message("Subj %s: Xmat '%s' contains %d of %d requested stimuli (%s)",
                     src->subj[ii],xm->fname,nfound,nrequested,
                     (stims!=NULL)?stims:"all") ; ok=0 ; goto row_done ;
     }
     { size_t nn=1,pp=0 ;
       for( jj=0 ; jj<xm->nstim ; jj++ ) if( want[jj] ) nn+=strlen(xm->stim[jj].label)+1 ;
       order=(char *)malloc(nn) ;
       if( order==NULL ){ ok=0 ; goto row_done ; }
       for( jj=0 ; jj<xm->nstim ; jj++ ) if( want[jj] ){
         if( pp>0 ) order[pp++]=',' ;
         pp+=(size_t)snprintf(order+pp,nn-pp,"%s",xm->stim[jj].label) ;
       }
     }
     it=THD_xmat_items(xm,want,&nitem) ;
     if( it==NULL || nitem<1 ){
       ERROR_message("Subj %s: cannot derive stimulus beta items from Xmat '%s'",
                     src->subj[ii],xm->fname) ; ok=0 ; goto row_done ;
     }
     ds=THD_open_dataset((char *)bucket) ;
     if( ds==NULL ){
       ERROR_message("Subj %s: cannot open whole InputFile '%s'",src->subj[ii],bucket) ;
       ok=0 ; goto row_done ;
     }
     brick=(int *)malloc(sizeof(int)*(size_t)nitem) ;
     if( brick==NULL ){ ok=0 ; goto row_done ; }
     for( jj=0 ; jj<nitem ; jj++ ){
       int bb ;
       if( it[jj].run<0 ){
         ERROR_message("Subj %s: Xmat regressor '%s' is not confined to one run;\n"
                       "       use an explicit -runwiseTable for this design",
                       src->subj[ii],it[jj].key) ; ok=0 ; break ;
       }
       if( strchr(it[jj].condition,',')!=NULL || strpbrk(it[jj].condition," \t\r\n")!=NULL ){
         ERROR_message("Subj %s: condition label '%s' cannot contain commas or whitespace",
                       src->subj[ii],it[jj].condition) ; ok=0 ; break ;
       }
       for( bb=0 ; bb<DSET_NVALS(ds) ; bb++ )
         if( DSET_BRICK_LABEL(ds,bb)!=NULL &&
             !strcmp(DSET_BRICK_LABEL(ds,bb),it[jj].coef_label) ) break ;
       if( bb==DSET_NVALS(ds) ){
         ERROR_message("Subj %s: InputFile '%s' has no brick labelled %s",
                       src->subj[ii],bucket,it[jj].coef_label) ; ok=0 ; break ;
       }
       brick[jj]=bb ;
     }
     for( rr=0 ; rr<xm->nrun && ok ; rr++ ){
       char rn[24],*sel,*conds ; size_t sc=strlen(bucket)+(size_t)nitem*24+4,cc=1,sp,cp ; int nrunitem=0 ;
       for( jj=0 ; jj<nitem ; jj++ ) if( it[jj].run==rr ){
         nrunitem++ ; cc+=strlen(it[jj].condition)+1 ;
       }
       if( nrunitem<1 ){
         ERROR_message("Subj %s: Xmat '%s' run %d has no selected stimulus beta",
                       src->subj[ii],xm->fname,rr+1) ; ok=0 ; break ;
       }
       sel=(char *)malloc(sc) ; conds=(char *)malloc(cc) ;
       if( sel==NULL || conds==NULL ){ free(sel) ; free(conds) ; ok=0 ; break ; }
       sp=(size_t)snprintf(sel,sc,"%s[",bucket) ; cp=0 ;
       for( jj=0 ; jj<nitem ; jj++ ) if( it[jj].run==rr ){
         if( cp>0 ){ sel[sp++]=',' ; conds[cp++]=',' ; }
         sp+=(size_t)snprintf(sel+sp,sc-sp,"%d",brick[jj]) ;
         cp+=(size_t)snprintf(conds+cp,cc-cp,"%s",it[jj].condition) ;
       }
       snprintf(sel+sp,sc-sp,"]") ; snprintf(rn,sizeof(rn),"%d",rr+1) ;
       ok &= runset_tok_add(&t,src->subj[ii]) ; ok &= runset_tok_add(&t,rn) ;
       ok &= runset_tok_add(&t,DT_CELL(src,ii,ix)) ;
       if( has_resid ) ok &= runset_tok_add(&t,DT_CELL(src,ii,ir)) ;
       ok &= runset_tok_add(&t,conds) ; ok &= runset_tok_add(&t,order) ;
       ok &= runset_tok_add(&t,sel) ;
       free(conds) ; free(sel) ;
     }
row_done:
     free(order) ; free(brick) ; if( ds!=NULL ) DSET_delete(ds) ;
     THD_xmat_items_free(it,nitem) ; free(want) ; THD_xmat_free(xm) ;
   }
   if( !ok ) goto done ;
   expanded=THD_parse_datatable(t.v,t.n,"expanded from xmatTable") ;
   if( expanded==NULL ){ ok=0 ; goto done ; }
   runset_tok_free(&t) ;
   THD_free_datatable(src) ; src=NULL ;
   out=runset_read_table(expanded,fname) ; expanded=NULL ;
done:
   runset_tok_free(&t) ;
   if( src!=NULL ) THD_free_datatable(src) ;
   if( expanded!=NULL ) THD_free_datatable(expanded) ;
   return ok ? out : NULL ;
}

int THD_runset_plan_store( THD_runset *rs, THD_roilist *primary,
                           THD_roilist *extra, int include_resid )
{
   unsigned char *use ; int ii,jj,kk,nwork=0 ;
   double native=0.0,compact=0.0,largest=0.0 ;
   THD_roilist *list[2] ;
   if( rs==NULL || primary==NULL || rs->nvox<1 ) return -1 ;
   if( rs->store_nvox>0 ) return 1 ;
   list[0]=primary ; list[1]=extra ;
   use=(unsigned char *)calloc((size_t)rs->nvox,1) ;
   if( use==NULL ) return -1 ;
   for( ii=0 ; ii<2 ; ii++ ) if( list[ii]!=NULL )
     for( kk=0 ; kk<list[ii]->nroi ; kk++ )
       for( jj=0 ; jj<list[ii]->vox[kk].nar ; jj++ ){
         int vv=list[ii]->vox[kk].ar[jj] ;
         if( vv<0 || vv>=rs->nvox ){ free(use) ; return -1 ; }
         if( !use[vv] ){ use[vv]=1 ; nwork++ ; }
       }
   if( nwork<1 ){ free(use) ; return -1 ; }
   for( ii=0 ; ii<rs->nrow ; ii++ ){
     double one=0.0 ; int bb ;
     for( bb=0 ; bb<DSET_NVALS(rs->betas[ii]) ; bb++ )
       one+=(double)DSET_BRICK_BYTES(rs->betas[ii],bb) ;
     native+=one ; if( one>largest ) largest=one ;
     compact+=(double)DSET_NVALS(rs->betas[ii])*nwork*sizeof(float) ;
     if( include_resid && rs->resid!=NULL ){
       one=0.0 ;
       for( bb=0 ; bb<DSET_NVALS(rs->resid[ii]) ; bb++ )
         one+=(double)DSET_BRICK_BYTES(rs->resid[ii],bb) ;
       native+=one ; if( one>largest ) largest=one ;
       compact+=(double)DSET_NVALS(rs->resid[ii])*nwork*sizeof(float) ;
     }
   }
   if( compact>=native ){ free(use) ; return 0 ; }
   rs->store_map=(int *)malloc(sizeof(int)*(size_t)rs->nvox) ;
   rs->store_vox=(int *)malloc(sizeof(int)*(size_t)nwork) ;
   rs->beta_store=(float **)calloc((size_t)rs->nrow,sizeof(float *)) ;
   rs->resid_store=include_resid ?
                   (float **)calloc((size_t)rs->nrow,sizeof(float *)) : NULL ;
   if( rs->store_map==NULL || rs->store_vox==NULL || rs->beta_store==NULL ||
       (include_resid && rs->resid_store==NULL) ){
     free(use) ; free(rs->store_map) ; free(rs->store_vox) ;
     free(rs->beta_store) ; free(rs->resid_store) ;
     rs->store_map=NULL ; rs->store_vox=NULL ;
     rs->beta_store=NULL ; rs->resid_store=NULL ; return -1 ;
   }
   for( ii=0,jj=0 ; ii<rs->nvox ; ii++ ){
     rs->store_map[ii]=use[ii] ? jj : -1 ;
     if( use[ii] ) rs->store_vox[jj++]=ii ;
   }
   free(use) ; rs->store_nvox=nwork ; rs->store_bytes=compact ;
   rs->store_largest_input=largest ; return 1 ;
}

int THD_runset_load_store_row( THD_runset *rs, int row, int residual )
{
   THD_3dim_dataset *ds ; float **slot,*out ; intvec vox ; size_t n ;
   if( rs==NULL || rs->store_nvox<1 || row<0 || row>=rs->nrow ) return 0 ;
   if( residual ){
     if( rs->resid==NULL || rs->resid_store==NULL ) return 0 ;
     ds=rs->resid[row] ; slot=rs->resid_store+row ;
   } else { ds=rs->betas[row] ; slot=rs->beta_store+row ; }
   if( *slot!=NULL ) return 1 ;
   n=(size_t)DSET_NVALS(ds)*rs->store_nvox ;
   out=(float *)malloc(sizeof(float)*n) ;
   if( out==NULL ) return 0 ;
   DSET_load(ds) ;
   if( !DSET_LOADED(ds) ){ free(out) ; return 0 ; }
   memset(&vox,0,sizeof(vox)) ; vox.nar=rs->store_nvox ; vox.ar=rs->store_vox ;
   THD_roi_pattern(ds,&vox,out) ; DSET_unload(ds) ; *slot=out ; return 1 ;
}

void THD_runset_roi_pattern( THD_runset *rs, int row, int residual,
                             intvec *vox, float *out )
{
   THD_3dim_dataset *ds ; float *src ; int nvx,nval,bb,ii ;
   if( rs==NULL || row<0 || row>=rs->nrow || vox==NULL || out==NULL ) return ;
   ds=residual ? rs->resid[row] : rs->betas[row] ;
   if( rs->store_nvox<1 ){ THD_roi_pattern(ds,vox,out) ; return ; }
   src=residual ? rs->resid_store[row] : rs->beta_store[row] ;
   if( src==NULL ) ERROR_exit("runwise compact store row %d was not loaded",row+1) ;
   nvx=vox->nar ; nval=DSET_NVALS(ds) ;
   for( bb=0 ; bb<nval ; bb++ ) for( ii=0 ; ii<nvx ; ii++ ){
     int vv=vox->ar[ii],ww=(vv>=0 && vv<rs->nvox)?rs->store_map[vv]:-1 ;
     if( ww<0 ) ERROR_exit("runwise compact store is missing voxel/node %d",vv) ;
     out[(size_t)bb*nvx+ii]=src[(size_t)bb*rs->store_nvox+ww] ;
   }
}

void THD_runset_roi_condition_pattern(THD_runset *rs,int row,intvec *vox,float *pat,float *raw)
{
   int b,c,v,nv=vox->nar;
   if(!rs->has_condmap) { THD_runset_roi_pattern(rs,row,0,vox,pat); return; }
   THD_runset_roi_pattern(rs,row,0,vox,raw);
   memset(pat,0,sizeof(float)*(size_t)rs->ncond*nv);
   for(b=0;b<rs->nbrick[row];b++) {
     c=rs->cond_of[row][b];
     for(v=0;v<nv;v++) pat[(size_t)c*nv+v]+=raw[(size_t)b*nv+v]/rs->nrep[row][c];
   }
}

void THD_runset_free( THD_runset *rs )
{
   int i , s ;
   if( rs == NULL ) return ;
   if( rs->beta_store!=NULL ){
     for( i=0 ; i<rs->nrow ; i++ ) free(rs->beta_store[i]) ;
     free(rs->beta_store) ;
   }
   if( rs->resid_store!=NULL ){
     for( i=0 ; i<rs->nrow ; i++ ) free(rs->resid_store[i]) ;
     free(rs->resid_store) ;
   }
   free(rs->store_map) ; free(rs->store_vox) ;
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
   fprintf(fp,"# %s: %s\n",rs->from_xmat?"xmatTable":"runwiseTable",
           (rs->source!=NULL)?rs->source:"?") ;
   fprintf(fp,"#   subjects   : %d\n",rs->nsub) ;
   if( rs->from_xmat ){
     int ii,minb=INT_MAX,maxb=0 ;
     for( ii=0 ; ii<rs->nrow ; ii++ ){
       if( rs->nbrick[ii]<minb ) minb=rs->nbrick[ii] ;
       if( rs->nbrick[ii]>maxb ) maxb=rs->nbrick[ii] ;
     }
     fprintf(fp,"#   conditions : %d named (Xmat; %d..%d beta bricks/run%s)\n",
             rs->ncond,minb,maxb,rs->has_condmap?"; repeats averaged":"") ;
     fprintf(fp,"#   cond order :") ;
     for( ii=0 ; ii<rs->ncond ; ii++ ) fprintf(fp," %s",rs->cond_lab[ii]) ;
     fprintf(fp,"\n") ;
   } else if( rs->has_condmap ){
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
