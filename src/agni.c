#include "mrilib.h"
#ifdef ALLOW_AGNI

/*------------------------------------------------------------------
  Create an empty surface description
--------------------------------------------------------------------*/

#define AGNI_EXTEND_NUM 64
#define AGNI_EXTEND_FAC 1.1

AGNI_surface * AGNI_create_empty_surface(void)
{
   AGNI_surface *ag ;

   ag = (AGNI_surface *) calloc(1,sizeof(AGNI_surface)) ;
   ag->num_nod  = ag->num_tri  = 0 ;
   ag->nall_nod = ag->nall_tri = 1 ;
   ag->nod = (AGNI_nod *) malloc(sizeof(AGNI_nod)) ; /* space for */
   ag->tri = (AGNI_tri *) malloc(sizeof(AGNI_tri)) ; /* 1 of each */

   if( ag->nod == NULL || ag->tri == NULL ){
      fprintf(stderr,"AGNI_create_empty_surface: can't malloc!\n"); exit(1);
   }

   ag->sparent = NULL ;

   ag->xbot = ag->ybot = ag->zbot =  WAY_BIG ;
   ag->xtop = ag->ytop = ag->ztop = -WAY_BIG ;

   ag->seq = ag->seqbase = ag->sorted = 0 ; /* not sequential; not sorted */

   return ag ;
}

/*------------------------------------------------------------------
  Throw out some trash
--------------------------------------------------------------------*/

void AGNI_destroy_surface( AGNI_surface *ag )
{
   if( ag == NULL ) return ;
   if( ag->nod != NULL ) free(ag->nod) ;
   if( ag->tri != NULL ) free(ag->tri) ;

   free(ag) ;
}

/*------------------------------------------------------------------
  Add a bunch of nodes to a surface
--------------------------------------------------------------------*/

void AGNI_add_nodes_ixyz( AGNI_surface *ag, int nadd,
                          int *iadd, float *xadd, float *yadd, float *zadd )
{
   int ii , nup ;

   if( ag == NULL || nadd < 1 ) return ;
   if( xadd == NULL || yadd == NULL || zadd == NULL || iadd == NULL ) return ;

   nup = ag->num_nod + nadd ;
   if( nup > ag->nall_nod ){ /* extend length of array */
      ag->nall_nod = nup = nup*AGNI_EXTEND_FAC + AGNI_EXTEND_NUM ;
      ag->nod = (AGNI_nod *) realloc( ag->nod , sizeof(AGNI_nod)*nup ) ;
      if( ag->nod == NULL ){
         fprintf(stderr,"AGNI_add_nodes_ixyz: can't malloc!\n"); exit(1);
      }
   }

   nup = ag->num_nod ;

   for( ii=0 ; ii < nadd ; ii++ ){
      ag->nod[ii+nup].x  = xadd[ii] ;
      ag->nod[ii+nup].y  = yadd[ii] ;
      ag->nod[ii+nup].z  = zadd[ii] ;
      ag->nod[ii+nup].id = iadd[ii] ;
   }

   ag->num_nod += nadd ;

   ag->seq = ag->sorted = 0 ;
}

/*------------------------------------------------------------------
  Add 1 pitiful node to a surface
--------------------------------------------------------------------*/

void AGNI_add_node_ixyz( AGNI_surface *ag , int i,float x,float y,float z )
{
   AGNI_add_nodes_ixyz( ag , 1 , &i,&x,&y,&z ) ;
}

/*------------------------------------------------------------------
  Add a bunch of triangles (node id triples) to a surface
--------------------------------------------------------------------*/

void AGNI_add_triangles( AGNI_surface *ag, int nadd, int *it, int *jt, int *kt )
{
   int ii , nup ;

   if( ag == NULL || nadd < 1 ) return ;
   if( it == NULL || jt == NULL || kt == NULL ) return ;

   nup = ag->num_tri + nadd ;
   if( nup > ag->nall_tri ){ /* extend length of array */
      ag->nall_tri = nup = nup*AGNI_EXTEND_FAC + AGNI_EXTEND_NUM ;
      ag->tri = (AGNI_tri *) realloc( ag->tri , sizeof(AGNI_tri)*nup ) ;
      if( ag->tri == NULL ){
         fprintf(stderr,"AGNI_add_triangles: can't malloc!\n"); exit(1);
      }
   }

   nup = ag->num_tri ;
   for( ii=0 ; ii < nadd ; ii++ ){
      ag->tri[ii+nup].i = it[ii] ;
      ag->tri[ii+nup].j = jt[ii] ;
      ag->tri[ii+nup].k = kt[ii] ;
   }

   ag->num_tri += nadd ;
}

/*------------------------------------------------------------------
  Add 1 pitiful triangle to a surface
--------------------------------------------------------------------*/

void AGNI_add_triangle( AGNI_surface *ag, int it, int jt, int kt )
{
   AGNI_add_triangles( ag , 1 , &it,&jt,&kt ) ;
}

/*------------------------------------------------------------------
  Truncate the memory used by the node and triangle arrays back
  to the minimum they need
--------------------------------------------------------------------*/

void AGNI_truncate_memory( AGNI_surface *ag )
{
   int nn ;

   if( ag == NULL ) return ;

   if( ag->num_nod < ag->nall_nod && ag->num_nod > 0 ){
      ag->nall_nod = nn = ag->num_nod ;
      ag->nod = (AGNI_nod *) realloc( ag->nod , sizeof(AGNI_nod)*nn ) ;
   }

   if( ag->num_tri < ag->nall_tri && ag->num_tri > 0 ){
      ag->nall_tri = nn = ag->num_tri ;
      ag->tri = (AGNI_tri *) realloc( ag->tri , sizeof(AGNI_tri)*nn ) ;
   }
}

/*------------------------------------------------------------------
  Generate a function to sort array of AGNI_nod-s by their id-s
--------------------------------------------------------------------*/

#define STYPE     AGNI_nod
#define SLT(a,b)  ((a).id < (b).id)
#define SNAME     AGNI_nod
#include "cs_sort_template.h"

/*------------------------------------------------------------------
  Sort the nod-s by id-s, and mark if the id-s are sequential
--------------------------------------------------------------------*/

void AGNI_nodesort_surface( AGNI_surface *ag )
{
   int nn , ii ;
   float xb,yb,zb , xt,yt,zt ;

   if( ag == NULL || ag->num_nod < 1 ) return ;

   AGNI_truncate_memory( ag ) ;

   nn = ag->num_nod ;
   qsort_AGNI_nod( nn , ag->nod ) ; ag->sorted = 1 ;

   /* check if node id-s are sequential */

   for( ii=1 ; ii < nn ; ii ++ )
      if( ag->nod[ii].id != ag->nod[ii-1].id+1 ) break ;

   if( ii == nn ){
      ag->seq = 1 ; ag->seqbase = ag->nod[0].id ;
   }

   /* find bounding box of all nodes */

   xb = xt = ag->nod[0].x ;
   yb = yt = ag->nod[0].y ;
   zb = zt = ag->nod[0].z ;
   for( ii=1 ; ii < nn ; ii++ ){
           if( ag->nod[ii].x < xb ) xb = ag->nod[ii].x ;
      else if( ag->nod[ii].x > xt ) xt = ag->nod[ii].x ;

           if( ag->nod[ii].y < yb ) yb = ag->nod[ii].y ;
      else if( ag->nod[ii].y > yt ) yt = ag->nod[ii].y ;

           if( ag->nod[ii].z < zb ) zb = ag->nod[ii].z ;
      else if( ag->nod[ii].z > zt ) zt = ag->nod[ii].z ;
   }

   ag->xbot = xb ; ag->xtop = xt ;
   ag->ybot = yb ; ag->ytop = yt ;
   ag->zbot = zb ; ag->ztop = zt ;
}

/*--------------------------------------------------------------------
   Find a node id in a surface, and return its index into the node
   array; return -1 if not found
----------------------------------------------------------------------*/

int AGNI_find_node_id( AGNI_surface *ag , int target )
{
   int nn , ii,jj,kk ;

   if( ag == NULL || ag->num_nod < 1 || target < 0 ) return -1 ;

   if( !ag->sorted ) AGNI_nodesort_surface( ag ) ;

   if( ag->seq ){  /* node id-s are sequential (the easy case) */

      kk = target - ag->seqbase ;
      if( kk >= 0 && kk < ag->num_nod ) return kk ;
      return -1 ;
   }

   /* node id-s are in increasing order, but not sequential;
      so, use binary search to find the node id (if present) */

   ii = 0 ; jj = ag->num_nod - 1 ;                 /* search bounds */

        if( target <  ag->nod[0].id  ) return -1 ; /* not present */
   else if( target == ag->nod[0].id  ) return ii ; /* at start!  */

        if( target >  ag->nod[jj].id ) return -1 ; /* not present */
   else if( target == ag->nod[jj].id ) return jj ; /* at end!    */

   while( jj - ii > 1 ){  /* while search bounds not too close */

      kk = (ii+jj) / 2 ;  /* midway between search bounds */

      nn = ag->nod[kk].id - target ;
      if( nn == 0 ) return kk ;       /* AHA! */

      if( nn < 0 ) ii = kk ;          /* kk before target => bottom = kk */
      else         jj = kk ;          /* kk after target  => top    = kk */
   }

   return -1 ;
}

/*--------------------------------------------------------------------------
  Read a surface description file; return a surface ready to rock-n-roll;
  NULL is returned if something bad happens
----------------------------------------------------------------------------*/

#define NBUF 64

AGNI_surface * AGNI_read_surface( char *fname )
{
   AGNI_surface *ag ;
   FILE *fp ;
   char lbuf[1024] , *cpt ;
   int nnod=0, ntri=0 , do_nod=1 , ii ;
   float xx[NBUF],yy[NBUF],zz[NBUF] ;
   int   pp[NBUF],qq[NBUF],rr[NBUF] , nn ;
   double ct ;

   if( fname == NULL || fname[0] == '\0' ) return NULL ;

   /*-- open input --*/

   if( strcmp(fname,"-") == 0 ){
      fp = stdin ;
   } else {
      fp = fopen( fname , "r" ) ;
      if( fp == NULL ) return NULL ;
   }

   /*-- read data --*/

   ag = AGNI_create_empty_surface() ;

ct = COX_cpu_time() ;

   nn = 0 ;

   while(1){
      cpt = fgets(lbuf,1024,fp) ;
      if( cpt == NULL ) break ;
      if( strstr(lbuf,"</NODES>") != NULL ){
         if( nn > 0 ){
            AGNI_add_nodes_ixyz( ag,nn , pp,xx,yy,zz ) ;
            nn = 0 ;
         }
#if 0
         do_nod = 0 ; continue ;  /* create triangles */
#else
         break ;                  /* don't create triangles */
#endif
      }
      if( do_nod ){
         ii = sscanf(lbuf,"%d%f%f%f",pp+nn,xx+nn,yy+nn,zz+nn) ;
         if( ii < 4 ) continue ;
         nn++ ; nnod++ ;
         if( nn == NBUF ){
            AGNI_add_nodes_ixyz( ag,nn , pp,xx,yy,zz ) ;
            nn = 0 ;
         }
      } else {
         ii = sscanf(lbuf,"%d%d%d",pp+nn,qq+nn,rr+nn) ;
         if( ii < 3 ) continue ;
         nn++ ; ntri++ ;
         if( nn == NBUF ){
            AGNI_add_triangles( ag,nn , pp,qq,rr ) ;
            nn = 0 ;
         }
      }
   }
   if( fp != stdin ) fclose(fp) ;
   if( nn > 0 ){
      if( do_nod )
         AGNI_add_nodes_ixyz( ag,nn , pp,xx,yy,zz ) ;
      else
         AGNI_add_triangles( ag,nn , pp,qq,rr ) ;
   }

   if( nnod < 3 ){
      AGNI_destroy_surface(ag) ; return NULL ;
   }

#if 0
fprintf(stderr,"AGNI_read_surface(%s): I/O CPU=%g\n",fname,COX_cpu_time()-ct) ;
ct = COX_cpu_time() ;
#endif

   AGNI_nodesort_surface(ag) ;

#if 0
fprintf(stderr,"   num_nod=%d seq=%d seqbase=%d nodesort CPU=%g\n"
               "   xbot=%g xtop=%g  ybot=%g ytop=%g  zbot=%g ztop=%g\n" ,
        ag->num_nod,ag->seq,ag->seqbase , COX_cpu_time()-ct,
        ag->xbot,ag->xtop , ag->ybot,ag->ytop , ag->zbot,ag->ztop ) ;
#endif

   /*-- done --*/

   return ag ;
}

/*-----------------------------------------------------------------------------*/

static int ip[26][3] = { {-1,-1,-1},{-1,-1, 0},{-1,-1, 1},
                         {-1, 0,-1},{-1, 0, 0},{-1, 0, 1},
                         {-1, 1,-1},{-1, 1, 0},{-1, 1, 1},
                         { 0,-1,-1},{ 0,-1, 0},{ 0,-1, 1},
                         { 0, 0,-1},           { 0, 0, 1},
                         { 0, 1,-1},{ 0, 1, 0},{ 0, 1, 1},
                         { 1,-1,-1},{ 1,-1, 0},{ 1,-1, 1},
                         { 1, 0,-1},{ 1, 0, 0},{ 1, 0, 1},
                         { 1, 1,-1},{ 1, 1, 0},{ 1, 1, 1} } ;

int * AGNI_map_vol_to_surf( AGNI_surface *ag ,
                            int nx    , int ny    , int nz    ,
                            float xoff, float yoff, float zoff,
                            float dx  , float dy  , float dz   )
{
   int *vmap , ii,jj,kk , nxy,nxyz , pp,qq , lev , ijk,pbest ;
   float dbest=0 , xv,yv,zv , sx,sy,sz ;

   if( ag == NULL || ag->num_nod < 1 ) return NULL ;

   if( nx < 3    || ny < 3    || nz < 3    ) return NULL ;
   if( dx == 0.0 || dy == 0.0 || dz == 0.0 ) return NULL ;

   /* setup */

   nxy = nx*ny ; nxyz = nxy*nz ;
   vmap = (int *) malloc(sizeof(int)*nxyz) ;
   if( vmap == NULL ){
      fprintf(stderr,"AGNI_map_vol_to_surf: can't malloc!\n"); exit(1);
   }
   for( ijk=0 ; ijk < nxyz ; ijk++ ) vmap[ijk] = -1 ;

   sx = 1.0/dx ; sy = 1.0/dy ; sz = 1.0/dz ;

   /* put nodes directly into voxels */

   for( pp=0 ; pp < ag->num_nod ; pp++ ){
      ii = (ag->nod[pp].x - xoff)*sx + 0.499 ;
      if( ii < 0 || ii >= nx ) continue ;

      jj = (ag->nod[pp].y - yoff)*sy + 0.499 ;
      if( jj < 0 || jj >= ny ) continue ;

      kk = (ag->nod[pp].z - zoff)*sz + 0.499 ;
      if( kk < 0 || kk >= nz ) continue ;

      vmap[ii+jj*nx+kk*nxy] = ag->nod[pp].id ;
   }

#if 0
   /* scan for voxels that are next to those already mapped */

   for( lev=1 ; lev < 4 ; lev++ ){

      for( kk=1 ; kk < nz-1 ; kk++ ){  /* only do interior voxels */
       for( jj=1 ; jj < ny-1 ; jj++ ){
        for( ii=1 ; ii < nx-1 ; ii++ ){
          ijk = ii+jj*nx+kk*nxy ;
          if( vmap[ijk] >= 0 ) continue ; /* already mapped */

          xv = xoff+ii*dx ; yv = yoff+ii*dy ; zv = zoff+ii*dz ;

          for( pbest=-1,qq=0 ; qq < 26 ; qq++ ){ /* loop over neighbors */

             pp = vmap[(ii+ip[qq][0]) + (jj+ip[qq][1])*nx + (kk+ip[qq][2])*nxy];

             if( pp >= 0 ){
                float xp=xv-ag->nod[pp].x, yp=yv-ag->nod[pp].y,
                                           zp=zv-ag->nod[pp].z ;
                float dd=xp*xp+yp*yp+zp*zp ;
                if( pbest >= 0 ){
                   if( dd < dbest ){ pbest = pp ; dbest = dd ; }
                } else {
                   pbest = pp ; dbest = dd ;
                }
             }
          }

          if( pbest >= 0 ) vmap[ijk] = pbest ;

      }}} /* end of loop over interior voxels */
   } /* end of loop over level */
#endif

   return vmap ;
}

/*------------------------------------------------------------------------*/

int * AGNI_map_dset_to_surf( AGNI_surface *ag , THD_3dim_dataset *dset )
{
   int *vmap , ii,jj,kk , nx,ny,nz , nxy,nxyz , pp ;
   THD_fvec3 fv ;
   THD_ivec3 iv ;

   if( ag == NULL || ag->num_nod < 1 || !ISVALID_DSET(dset) ) return NULL ;

   /* setup */

   nx = DSET_NX(dset) ; ny = DSET_NY(dset) ; nz = DSET_NZ(dset) ;
   nxy = nx*ny ; nxyz = nxy*nz ;
   vmap = (int *) malloc(sizeof(int)*nxyz) ;
   if( vmap == NULL ){
      fprintf(stderr,"AGNI_map_dset_to_surf: can't malloc!\n"); exit(1);
   }
   for( ii=0 ; ii < nxyz ; ii++ ) vmap[ii] = -1 ;

   /* put nodes directly into voxels */

   for( pp=0 ; pp < ag->num_nod ; pp++ ){
      LOAD_FVEC3( fv , ag->nod[pp].x , ag->nod[pp].y , ag->nod[pp].z ) ;
      fv = THD_dicomm_to_3dmm( dset , fv ) ;
      iv = THD_3dmm_to_3dind( dset , fv ) ;
      UNLOAD_IVEC3( iv , ii,jj,kk ) ;
      vmap[ii+jj*nx+kk*nxy] = ag->nod[pp].id ;
   }

   return vmap ;
}

/*-------------------------------------------------------------------------*/

void AGNI_get_sname( THD_3dim_dataset *dset )
{
   char *snam ;
   int ii ;

   if( !ISVALID_DSET(dset) || dset->ag_sname != NULL ) return ;

   snam = strdup( DSET_HEADNAME(dset) ) ;
   ii = strlen(snam) ;
   if( ii > 5 ){
      strcpy(snam+ii-4,"SURF") ;
      if( THD_is_file(snam) ){ dset->ag_sname = snam; return; }
   }
   free(snam) ; return ;
}

/*-------------------------------------------------------------------------*/

void AGNI_load( THD_3dim_dataset *dset )
{
   double ct ;

   if( !ISVALID_DSET(dset)    ||
       dset->ag_sname == NULL || dset->ag_surf != NULL ) return ;

   dset->ag_surf = AGNI_read_surface( dset->ag_sname ) ;

   if( dset->ag_surf == NULL ){
      free(dset->ag_sname) ; dset->ag_sname = NULL ; return ;
   }

#if 0
ct = COX_cpu_time() ;
   dset->ag_vmap = AGNI_map_dset_to_surf( dset->ag_surf , dset ) ;
fprintf(stderr,"AGNI_map_dset_to_surf CPU=%g\n",COX_cpu_time()-ct) ;
#endif
}

/*--------------------------------------------------------------------------*/

void AGNI_unload( THD_3dim_dataset *dset )
{
   if( !ISVALID_DSET(dset) || dset->ag_sname == NULL ) return ;

   if( dset->ag_surf != NULL ){
      AGNI_destroy_surface( dset->ag_surf ) ; dset->ag_surf = NULL ;
   }

   if( dset->ag_vmap != NULL ){
      free( dset->ag_vmap ) ; dset->ag_vmap = NULL ;
   }
}

/*===================*/
#endif /* ALLOW_AGNI */
