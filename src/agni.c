#include "agni.h"

/*------------------------------------------------------------------*/

static rgba AGNI_default_rgba = { 128,96,64,255 } ;

void AGNI_set_default_rgba( rgba val ){ AGNI_default_rgba = val ; }

/*------------------------------------------------------------------*/

#define AGNI_EXTEND_NUM 64
#define AGNI_EXTEND_FAC 1.1

AGNI_surface * AGNI_create_empty_surface(void)
{
   AGNI_surface *ag ;
   ag = (AGNI_surface *) malloc(sizeof(AGNI_surface)) ;
   ag->num_nod = ag->num_tri = 0 ;
   ag->nall_nod = ag->nall_tri = AGNI_EXTEND_NUM ;
   ag->nod = (AGNI_nod *) malloc(sizeof(AGNI_nod)*AGNI_EXTEND_NUM) ;
   ag->tri = (AGNI_tri *) malloc(sizeof(AGNI_tri)*AGNI_EXTEND_NUM) ;
   ag->clr = (rgba *    ) malloc(sizeof(rgba    )*AGNI_EXTEND_NUM) ;

   ag->spindex = NULL ; ag->sparent = NULL ; ag->sippsurf = NULL ;

   return ag ;
}

/*------------------------------------------------------------------*/

void AGNI_destroy_surface( AGNI_surface *ag )
{
   if( ag == NULL ) return ;
   if( ag->nod != NULL ) free(ag->nod) ;
   if( ag->tri != NULL ) free(ag->tri) ;
   if( ag->clr != NULL ) free(ag->clr) ;

   if( ag->spindex != NULL ) AGNI_destroy_spatial_index( ag->spindex ) ;

   if( ag->sippsurf != NULL ) surface_unref(ag->sippsurf) ;

   free(ag) ;
}

/*------------------------------------------------------------------*/

void AGNI_add_nodes_xyzcol( AGNI_surface *ag, int nadd,
                            float *xadd, float *yadd, float *zadd, rgba *cadd )
{
   int ii , nup ;

   if( ag == NULL || nadd < 1 ) return ;
   if( xadd == NULL || yadd == NULL || zadd == NULL || cadd == NULL ) return ;

   nup = ag->num_nod + nadd ;
   if( nup > ag->nall_nod ){ /* extend length of array */
      ag->nall_nod = nup = (nup + AGNI_EXTEND_NUM) * AGNI_EXTEND_FAC ;
      ag->nod = (AGNI_nod *) realloc( ag->nod , sizeof(AGNI_nod)*nup ) ;
      ag->clr = (rgba *    ) realloc( ag->clr , sizeof(rgba)    *nup ) ;
   }

   nup = ag->num_nod ;
   for( ii=0 ; ii < nadd ; ii++ ){
      ag->nod[ii+nup].x = xadd[ii] ;
      ag->nod[ii+nup].y = yadd[ii] ;
      ag->nod[ii+nup].z = zadd[ii] ;
      ag->clr[ii+nup]   = cadd[ii] ;
   }

   ag->num_nod += nadd ;
}

/*------------------------------------------------------------------*/

void AGNI_add_nodes_xyz( AGNI_surface *ag, int nadd,
                         float *xadd, float *yadd, float *zadd )
{
   rgba *ccc ;
   int ii ;

   if( ag == NULL || nadd < 1 ) return ;

   ccc = (rgba *) malloc(sizeof(rgba)*nadd) ;
   for( ii=0 ; ii < nadd ; ii++ ) ccc[ii] = AGNI_default_rgba ;

   AGNI_add_nodes_xyzcol( ag, nadd,xadd,yadd,zadd,ccc ) ;
   free(ccc) ;
}

/*------------------------------------------------------------------*/

void AGNI_add_node_xyzcol( AGNI_surface *ag , float x,float y,float z,rgba c )
{
   AGNI_add_nodes_xyzcol( ag , 1 , &x,&y,&z,&c ) ;
}

/*------------------------------------------------------------------*/

void AGNI_add_node_xyz( AGNI_surface *ag , float x, float y, float z )
{
   AGNI_add_node_xyzcol( ag , x,y,z,AGNI_default_rgba ) ;
}

/*------------------------------------------------------------------*/

void AGNI_truncate_memory( AGNI_surface *ag )
{
   int nn ;

   if( ag == NULL ) return ;

   if( ag->num_nod < ag->nall_nod ){
      ag->nall_nod = nn = ag->num_nod ;
      ag->nod = (AGNI_nod *) realloc( ag->nod , sizeof(AGNI_nod)*nn ) ;
      ag->clr = (rgba *    ) realloc( ag->nod , sizeof(rgba *  )*nn ) ;
   }

   if( ag->num_tri < ag->nall_tri ){
      ag->nall_tri = nn = ag->num_tri ;
      ag->tri = (AGNI_tri *) realloc( ag->tri , sizeof(AGNI_tri)*nn ) ;
   }
}

/*------------------------------------------------------------------*/

void AGNI_add_triangles( AGNI_surface *ag, int nadd, int *it, int *jt, int *kt )
{
   int ii , nup ;

   if( ag == NULL || nadd < 1 ) return ;
   if( it == NULL || jt == NULL || kt == NULL ) return ;

   nup = ag->num_tri + nadd ;
   if( nup > ag->nall_tri ){ /* extend length of array */
      ag->nall_tri = nup = (nup + AGNI_EXTEND_NUM) * AGNI_EXTEND_FAC ;
      ag->tri = (AGNI_tri *) realloc( ag->tri , sizeof(AGNI_tri)*nup ) ;
   }

   nup = ag->num_tri ;
   for( ii=0 ; ii < nadd ; ii++ ){
      ag->tri[ii+nup].i = it[ii] ;
      ag->tri[ii+nup].j = jt[ii] ;
      ag->tri[ii+nup].k = kt[ii] ;
   }

   ag->num_tri += nadd ;
}

/*------------------------------------------------------------------*/

void AGNI_add_triangle( AGNI_surface *ag, int it, int jt, int kt )
{
   AGNI_add_triangles( ag , 1 , &it,&jt,&kt ) ;
}

/*-------------------------------------------------------------------*/

void AGNI_create_spatial_index( AGNI_surface *ag )
{
   int ii,jj,kk,num,nall , nn,pp , mm , ngood ;
   float xb,yb,zb , xt,yt,zt , dx,dy,dz ;
   AGNI_spatial_index *sp ;

   if( ag == NULL || ag->num_nod < 1000 ) return ;

   xb = yb = zb = WAY_BIG ; xt = yt = zt = -WAY_BIG ;
   nn = ag->num_nod ;

   for( ngood=pp=0 ; pp < nn ; pp++ ){
      if( AGNI_GOOD_NOD(ag->nod[pp]) ){
         ngood++ ;
         dx = ag->nod[pp].x; dy = ag->nod[pp].y; dz = ag->nod[pp].z;
         if( dx < xb ) xb = dx ;
         if( dy < yb ) yb = dy ;
         if( dz < zb ) zb = dz ;
         if( dx > xt ) xt = dx ;
         if( dy > yt ) yt = dy ;
         if( dz > zt ) zt = dz ;
      }
   }
   if( ngood < 200 || xt <= xb || yt <= yb || zt <= zb ) return ;

   if( ag->spindex != NULL ) AGNI_destroy_spatial_index( ag->spindex ) ;
   ag->spindex = sp = (AGNI_spatial_index *) malloc(sizeof(AGNI_spatial_index)) ;

   sp->mx = sp->my = sp->mz = mm = (int)( pow(0.01*ngood,0.33333) + 0.999 ) ;

   sp->nlist = (int ****) malloc( sizeof(int ***)*mm ) ;
   for( ii=0 ; ii < mm ; ii++ ){
      sp->nlist[ii] = (int ***) malloc( sizeof(int **)*mm ) ;
      for( jj=0 ; jj < mm ; jj++ ){
         sp->nlist[ii][jj] = (int **) malloc( sizeof(int *)*mm ) ;
         for( kk=0 ; kk < mm ; kk++ ){
            sp->nlist[ii][jj][kk] = (int *) malloc(sizeof(int)*AGNI_EXTEND_NUM) ;
            sp->nlist[ii][jj][kk][0] = 2 ;
            sp->nlist[ii][jj][kk][1] = AGNI_EXTEND_NUM ;
         }
      }
   }

   dx = xt-xb ; dy = yt-yb ; dz = zt-zb ;
                      sp->eps = dx ;
   if( sp->eps < dy ) sp->eps = dy ;
   if( sp->eps < dz ) sp->eps = dz ;
   sp->eps *= 1.e-4 ;

   dx = sp->dx = mm/dx ; sp->xb = xb ;
   dy = sp->dy = mm/dy ; sp->yb = yb ;
   dz = sp->dz = mm/dz ; sp->zb = zb ;

   for( pp=0 ; pp < nn ; pp++ ){
      if( AGNI_GOOD_NOD(ag->nod[pp]) ){
         ii = dx*(ag->nod[pp].x - xb) ;
         jj = dy*(ag->nod[pp].y - yb) ;
         kk = dz*(ag->nod[pp].z - zb) ;
         num  = sp->nlist[ii][jj][kk][0] ;
         nall = sp->nlist[ii][jj][kk][1] ;
         if( num >= nall ){
            sp->nlist[ii][jj][kk][1] = nall =
               AGNI_EXTEND_FAC * nall + AGNI_EXTEND_NUM ;
            sp->nlist[ii][jj][kk] = (int *) realloc( sp->nlist[ii][jj][kk] ,
                                                     sizeof(int)*nall       );
         }
         sp->nlist[ii][jj][kk][num] = pp ;
         sp->nlist[ii][jj][kk][0]++ ;
      }
   }

   for( ii=0 ; ii < mm ; ii++ ){
      for( jj=0 ; jj < mm ; jj++ ){
         for( kk=0 ; kk < mm ; kk++ ){
            num = sp->nlist[ii][jj][kk][0] ;
            sp->nlist[ii][jj][kk] = (int *) realloc( sp->nlist[ii][jj][kk] ,
                                                     sizeof(int)*num        );
            sp->nlist[ii][jj][kk][1] = num ;
         }
      }
   }
}

/*-------------------------------------------------------------------*/

int AGNI_find_node( AGNI_surface *ag , float xx, float yy, float zz )
{
   int pp ;
   float qq,eps ;

   if( ag == NULL || ag->num_nod < 1 ) return -1 ;

   if( ag->spindex != NULL ){                /* clever search */
      AGNI_spatial_index *sp = ag->spindex ;
      int ii,jj,kk, nn , num ;

      eps = (sp->eps)*(sp->eps) ;

      ii = sp->dx*(xx - sp->xb) ; if(ii<0 || ii>=sp->mx) return -1 ;
      jj = sp->dy*(yy - sp->yb) ; if(jj<0 || jj>=sp->my) return -1 ;
      kk = sp->dz*(zz - sp->zb) ; if(kk<0 || kk>=sp->mz) return -1 ;
      num = sp->nlist[ii][jj][kk][0] ;
      for( nn=2 ; nn < num ; nn++ ){
         pp = sp->nlist[ii][jj][kk][nn] ;
         qq = (xx-ag->nod[pp].x)*(xx-ag->nod[pp].x)
             +(yy-ag->nod[pp].y)*(yy-ag->nod[pp].y)
             +(zz-ag->nod[pp].z)*(zz-ag->nod[pp].z) ;
         if( qq < eps ) return pp ;
      }
      return -1 ;
   }

   /* OK, brute force search */

   eps = 1.e-6 ;
   for( pp=0 ; pp < ag->num_nod ; pp++ ){
      qq = (xx-ag->nod[pp].x)*(xx-ag->nod[pp].x)
          +(yy-ag->nod[pp].y)*(yy-ag->nod[pp].y)
          +(zz-ag->nod[pp].z)*(zz-ag->nod[pp].z) ;
      if( qq < eps ) return pp ;
   }

   return -1 ;
}

/*-------------------------------------------------------------------*/

void AGNI_destroy_spatial_index( AGNI_spatial_index *sp )
{
   int ii,jj,kk ;

   if( sp == NULL ) return ;

   if( sp->nlist != NULL ){
      for( ii=0 ; ii < sp->mx ; ii++ ){
         for( jj=0 ; jj < sp->my ; jj++ ){
            for( kk=0 ; kk < sp->mz ; kk++ ){
               free(sp->nlist[ii][jj][kk]) ;
            }
            free(sp->nlist[ii][jj]) ;
         }
         free(sp->nlist[ii]) ;
      }
      free(sp->nlist) ;
   }
}
