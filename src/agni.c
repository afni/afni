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

ENTRY("AGNI_create_empty_surface") ;

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

   RETURN( ag ) ;
}

/*------------------------------------------------------------------
  Throw out some trash
--------------------------------------------------------------------*/

void AGNI_destroy_surface( AGNI_surface *ag )
{
ENTRY("AGNI_destroy_surface") ;

   if( ag == NULL ) EXRETURN ;
   if( ag->nod != NULL ) free(ag->nod) ;
   if( ag->tri != NULL ) free(ag->tri) ;

   free(ag) ; EXRETURN ;
}

/*------------------------------------------------------------------
  Add a bunch of nodes to a surface
--------------------------------------------------------------------*/

void AGNI_add_nodes_ixyz( AGNI_surface *ag, int nadd,
                          int *iadd, float *xadd, float *yadd, float *zadd )
{
   int ii , nup ;

ENTRY("AGNI_add_nodes_ixyz") ;

   if( ag == NULL || nadd < 1 ) EXRETURN ;
   if( xadd == NULL || yadd == NULL || zadd == NULL || iadd == NULL ) EXRETURN ;

   nup = ag->num_nod + nadd ;

   if( nup >= AGNI_MAX_NODES ){  /* 07 Sep 2001 */
      fprintf(stderr,
              "** AGNI surface can't have more than %d nodes!\n",
              AGNI_MAX_NODES-1 ) ;
      EXRETURN ;
   }

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

   ag->seq = ag->sorted = 0 ; EXRETURN ;
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

ENTRY("AGNI_add_triangles") ;

   if( ag == NULL || nadd < 1 ) EXRETURN ;
   if( it == NULL || jt == NULL || kt == NULL ) EXRETURN ;

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

   ag->num_tri += nadd ; EXRETURN ;
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

ENTRY("AGNI_truncate_memory") ;

   if( ag == NULL ) EXRETURN ;

   if( ag->num_nod < ag->nall_nod && ag->num_nod > 0 ){
      ag->nall_nod = nn = ag->num_nod ;
      ag->nod = (AGNI_nod *) realloc( ag->nod , sizeof(AGNI_nod)*nn ) ;
   }

   if( ag->num_tri < ag->nall_tri && ag->num_tri > 0 ){
      ag->nall_tri = nn = ag->num_tri ;
      ag->tri = (AGNI_tri *) realloc( ag->tri , sizeof(AGNI_tri)*nn ) ;
   }

   EXRETURN ;
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
   int nn , ii , ndup ;
   float xb,yb,zb , xt,yt,zt ;

ENTRY("AGNI_nodesort_surface") ;

   if( ag == NULL || ag->num_nod < 1 ) EXRETURN ;

   AGNI_truncate_memory( ag ) ;

   nn = ag->num_nod ;

   /* check if nodes are already sorted [26 Oct 2001] */

   for( ii=1 ; ii < nn ; ii++ )
      if( ag->nod[ii].id <= ag->nod[ii-1].id ) break ;

   /* if not in increasing order, sort them */

   if( ii < nn ){
fprintf(stderr,"AGNI: Sorting nodes") ;
      qsort_AGNI_nod( nn , ag->nod ) ;
fprintf(stderr," .. done\n") ;
   }

   ag->sorted = 1 ;  /* mark as sorted */

   /* check if node id-s are sequential */

   for( ii=1 ; ii < nn ; ii++ )
      if( ag->nod[ii].id != ag->nod[ii-1].id+1 ) break ;

   if( ii == nn ){
      ag->seq = 1 ; ag->seqbase = ag->nod[0].id ;
   }

   /* 07 Sep 2001: check for duplicate node id-s */

   for( ndup=0,ii=1 ; ii < nn ; ii++ )
      if( ag->nod[ii].id == ag->nod[ii-1].id ) ndup++ ;

   if( ndup > 0 )
      fprintf(stderr,"** AGNI: WARNING: duplicate surface node id's found!\n") ;

   /* find bounding box of all nodes (its useful on occasion) */

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

   EXRETURN ;
}

/*--------------------------------------------------------------------
   Find a node id in a surface, and return its index into the node
   array; return -1 if not found
----------------------------------------------------------------------*/

int AGNI_find_node_id( AGNI_surface *ag , int target )
{
   int nn , ii,jj,kk ;

ENTRY("AGNI_find_node_id") ;

   if( ag == NULL || ag->num_nod < 1 || target < 0 ) RETURN( -1 );

   if( !ag->sorted ) AGNI_nodesort_surface( ag ) ;

   if( ag->seq ){  /* node id-s are sequential (the easy case) */

      kk = target - ag->seqbase ;
      if( kk >= 0 && kk < ag->num_nod ) RETURN( kk );
      RETURN( -1 );
   }

   /* node id-s are in increasing order, but not sequential;
      so, use binary search to find the node id (if present) */

   ii = 0 ; jj = ag->num_nod - 1 ;                 /* search bounds */

        if( target <  ag->nod[0].id  ) RETURN( -1 ); /* not present */
   else if( target == ag->nod[0].id  ) RETURN( ii ); /* at start!  */

        if( target >  ag->nod[jj].id ) RETURN( -1 ); /* not present */
   else if( target == ag->nod[jj].id ) RETURN( jj ); /* at end!    */

   while( jj - ii > 1 ){  /* while search bounds not too close */

      kk = (ii+jj) / 2 ;  /* midway between search bounds */

      nn = ag->nod[kk].id - target ;
      if( nn == 0 ) RETURN( kk );       /* AHA! */

      if( nn < 0 ) ii = kk ;          /* kk before target => bottom = kk */
      else         jj = kk ;          /* kk after target  => top    = kk */
   }

   RETURN( -1 );
}

/*--------------------------------------------------------------------------
  Read a surface description file; return a surface ready to rock-n-roll;
  NULL is returned if something bad happens
----------------------------------------------------------------------------*/

#define NBUF 64

AGNI_surface * AGNI_read_surface( char *fname , THD_3dim_dataset *dset )
{
   AGNI_surface *ag ;
   FILE *fp ;
   char lbuf[1024] , *cpt ;
   int  do_nod=1 , ii ;
   float xx[NBUF],yy[NBUF],zz[NBUF] ;
   int   pp[NBUF],qq[NBUF],rr[NBUF] , nn ;

   THD_vecmat mv ;
   int have_mv=0 ;

ENTRY("AGNI_read_surface") ;

   if( fname == NULL || fname[0] == '\0' ) RETURN( NULL );

   /*-- open input --*/

   if( strcmp(fname,"-") == 0 ){
      fp = stdin ;
   } else {
      fp = fopen( fname , "r" ) ;
      if( fp == NULL ) RETURN( NULL );
fprintf(stderr,"\nAGNI: Reading surface file %s\n",fname) ;
   }

   /*-- read data --*/

   ag = AGNI_create_empty_surface() ;

   nn = 0 ;

   while(1){
      cpt = fgets(lbuf,1024,fp) ;  /* read a line */
      if( cpt == NULL ) break ;    /* end of file */

      /*-- read a transformation matrix-vector --*/

      if( strncmp(lbuf,"<MATVEC>",8) == 0 ){  /* 07 Sep 2001 */
         float a11,a12,a13 , v1 ,
               a21,a22,a23 , v2 ,
               a31,a32,a33 , v3  ;

         ii = sscanf( lbuf+8 , "%f%f%f%f%f%f%f%f%f%f%f%f" ,
                      &a11,&a12,&a13 , &v1 ,
                      &a21,&a22,&a23 , &v2 ,
                      &a31,&a32,&a33 , &v3  ) ;

         if( ii < 12 ){
            fprintf(stderr,"** AGNI: Illegal MATVEC in %s\n",fname) ;
            have_mv = 0 ;
         } else {
            LOAD_FVEC3(mv.vv , v1,v2,v3 ) ;
            LOAD_MAT  (mv.mm , a11,a12,a13,a21,a22,a23,a31,a32,a33) ;
            have_mv = 1 ;
         }
         continue ; /* skip to next line */
      }

      /*-- read data from SureFit? --*/

      if( strncmp(lbuf,"<SureFit",8) == 0 ){ /* 26 Oct 2001 */

         if( nn > 0 ){   /* process existing inputs, if any */
            if( do_nod )
               AGNI_add_nodes_ixyz( ag,nn , pp,xx,yy,zz ) ;
            else
               AGNI_add_triangles( ag,nn , pp,qq,rr ) ;
            nn = 0 ;
         }

         AGNI_import_surefit( ag , lbuf , dset ) ;
         continue ; /* skip to next input line */

      } /* end of SureFit input */

      /*-- end of node input? --*/

      if( strstr(lbuf,"</NODES>") != NULL ){
         if( do_nod && nn > 0 ){
            AGNI_add_nodes_ixyz( ag,nn , pp,xx,yy,zz ) ;
            nn = 0 ;
         }
#if 0
         do_nod = 0 ;  /* from now on, triangles */
         continue ;    /* skip to next line */
#else
         break ;                  /* don't create triangles */
#endif
      }

      /*-- process line as a node? --*/

      if( do_nod ){               /* nodes */

         ii = sscanf(lbuf,"%d%f%f%f",pp+nn,xx+nn,yy+nn,zz+nn) ;
         if( ii < 4 ) continue ;
         if( have_mv ){
            THD_fvec3 fv , qv ;
            LOAD_FVEC3(fv , xx[nn],yy[nn],zz[nn] ) ;
            qv = VECSUB_MAT( mv.mm , fv , mv.vv ) ;
            UNLOAD_FVEC3( qv , xx[nn],yy[nn],zz[nn] ) ;
         }
         nn++ ;
         if( nn == NBUF ){
            AGNI_add_nodes_ixyz( ag,nn , pp,xx,yy,zz ) ;
            nn = 0 ;
         }

      /*-- process line as a triangle --*/

      } else {                    /* triangles */

         ii = sscanf(lbuf,"%d%d%d",pp+nn,qq+nn,rr+nn) ;
         if( ii < 3 ) continue ;
         nn++ ;
         if( nn == NBUF ){
            AGNI_add_triangles( ag,nn , pp,qq,rr ) ;
            nn = 0 ;
         }
      }
   } /* end of loop over input lines */

   /*-- finish up, eh? --*/

   if( fp != stdin ) fclose(fp) ;
   if( nn > 0 ){
      if( do_nod )
         AGNI_add_nodes_ixyz( ag,nn , pp,xx,yy,zz ) ;
      else
         AGNI_add_triangles( ag,nn , pp,qq,rr ) ;
   }

   if( ag->num_nod < 1 ){
      AGNI_destroy_surface(ag) ; RETURN(NULL) ;
   }

   AGNI_nodesort_surface(ag) ;

   /*-- done --*/

   RETURN( ag );
}

/*-----------------------------------------------------------------------------*/

  /* 26 point 3x3x3 nbhd of {0,0,0} */

static int ip[26][3] = { {-1,-1,-1},{-1,-1, 0},{-1,-1, 1},
                         {-1, 0,-1},{-1, 0, 0},{-1, 0, 1},
                         {-1, 1,-1},{-1, 1, 0},{-1, 1, 1},
                         { 0,-1,-1},{ 0,-1, 0},{ 0,-1, 1},
                         { 0, 0,-1},           { 0, 0, 1},
                         { 0, 1,-1},{ 0, 1, 0},{ 0, 1, 1},
                         { 1,-1,-1},{ 1,-1, 0},{ 1,-1, 1},
                         { 1, 0,-1},{ 1, 0, 0},{ 1, 0, 1},
                         { 1, 1,-1},{ 1, 1, 0},{ 1, 1, 1} } ;

/*------------------------------------------------------------------------*/

int * AGNI_map_dset_to_surf( AGNI_surface *ag , THD_3dim_dataset *dset )
{
   int *vmap , ii,jj,kk , nx,ny,nz , nxy,nxyz , pp,qq,pbest ;
   THD_fvec3 fv ;
   THD_ivec3 iv ;
   int ibot,jbot,kbot , itop,jtop,ktop , lev , ijk ;
   float xv,yv,zv , dd,dbest=0 , xp,yp,zp ;
   char *elev ; int ltop , ntop , lmask ;

ENTRY("AGNI_map_dset_to_surf") ;

   if( ag == NULL || ag->num_nod < 1 || !ISVALID_DSET(dset) ) RETURN( NULL );

   /* setup */

fprintf(stderr,"AGNI: Mapping surface nodes to voxel indexes") ;

   nx = DSET_NX(dset) ; ny = DSET_NY(dset) ; nz = DSET_NZ(dset) ;
   nxy = nx*ny ; nxyz = nxy*nz ;
   vmap = (int *) malloc(sizeof(int)*nxyz) ;
   if( vmap == NULL ){
      fprintf(stderr,"AGNI_map_dset_to_surf: can't malloc!\n"); exit(1);
   }
   for( ii=0 ; ii < nxyz ; ii++ ) vmap[ii] = -1 ; /* not mapped yet */

   /* put nodes directly into voxels */

STATUS("putting nodes into voxels") ;

   for( pp=0 ; pp < ag->num_nod ; pp++ ){
      LOAD_FVEC3( fv , ag->nod[pp].x , ag->nod[pp].y , ag->nod[pp].z ) ;
      fv = THD_dicomm_to_3dmm( dset , fv ) ; /* convert Dicom */
      iv = THD_3dmm_to_3dind( dset , fv ) ;  /*   coords to dataset */
      UNLOAD_IVEC3( iv , ii,jj,kk ) ;        /*   indexes */
      qq = vmap[ii+jj*nx+kk*nxy] ;           /* previously mapped index? */
      if( qq < 0 ){                          /* not mapped before */
         vmap[ii+jj*nx+kk*nxy] = pp ;        /* index, not id */
      } else {
         LOAD_IVEC3(iv,ii,jj,kk) ;           /* get Dicom coords of voxel */
         fv = THD_3dind_to_3dmm( dset , iv ) ;
         fv = THD_3dmm_to_dicomm( dset , fv ) ;
         UNLOAD_FVEC3(fv,xv,yv,zv) ;

         xp=xv-ag->nod[qq].x ;
         yp=yv-ag->nod[qq].y ;
         zp=zv-ag->nod[qq].z ; dd=xp*xp+yp*yp+zp*zp ;    /* dist to old node */

         xp=xv-ag->nod[pp].x ;
         yp=yv-ag->nod[pp].y ;
         zp=zv-ag->nod[pp].z ; dbest=xp*xp+yp*yp+zp*zp ; /* dist to new node */

         if( dbest < dd ) vmap[ii+jj*nx+kk*nxy] = pp ;   /* new is better */
      }
   }

   LOAD_FVEC3( fv , ag->xbot,ag->ybot,ag->zbot ) ; /* find dataset */
   fv = THD_dicomm_to_3dmm( dset , fv ) ;          /* indexes for */
   iv = THD_3dmm_to_3dind( dset , fv ) ;           /* bot point  */
   UNLOAD_IVEC3( iv , ibot,jbot,kbot ) ;           /* of surface */

   LOAD_FVEC3( fv , ag->xtop,ag->ytop,ag->ztop ) ; /* and for top */
   fv = THD_dicomm_to_3dmm( dset , fv ) ;
   iv = THD_3dmm_to_3dind( dset , fv ) ;
   UNLOAD_IVEC3( iv , itop,jtop,ktop ) ;

   if( ibot > itop ){ ii=ibot ; ibot=itop; itop=ii; }
   if( jbot > jtop ){ jj=jbot ; jbot=jtop; jtop=jj; }
   if( kbot > ktop ){ kk=kbot ; kbot=ktop; ktop=kk; }
   if( ibot < 1 ) ibot = 1 ; if( itop >= nx ) itop = nx-1 ;
   if( jbot < 1 ) jbot = 1 ; if( jtop >= ny ) jtop = ny-1 ;
   if( kbot < 1 ) kbot = 1 ; if( ktop >= nz ) ktop = nz-1 ;

   ntop = ag->nod[ag->num_nod-1].id + 100 ;

fprintf(stderr,".") ;

   /* scan for voxels that are next to those already mapped */

   elev = getenv("AGNI_NBHD_LEVEL") ;  /* find level for expanding out */
   if( elev != NULL ){
      char *cpt ;
      ltop = strtol( elev , &cpt , 10 ) ;
      if( ltop < 0 || ltop > 7 || (ltop == 0 && *cpt != '\0') ) ltop = 4 ;
   } else {
      ltop = 4 ;
   }

   for( lev=1 ; lev <= ltop ; lev++ ){  /* if ltop = 0, won't be executed */

    if(PRINT_TRACING){
     char str[256]; sprintf(str,"expansion level %d",lev); STATUS(str);
    }

    for( kk=kbot ; kk < ktop ; kk++ ){
     for( jj=jbot ; jj < jtop ; jj++ ){
      for( ii=ibot ; ii < itop ; ii++ ){
        ijk = ii+jj*nx+kk*nxy ;
        if( vmap[ijk] >= 0 ) continue ; /* already mapped */

        LOAD_IVEC3(iv,ii,jj,kk) ;               /* get Dicom coords */
        fv = THD_3dind_to_3dmm( dset , iv ) ;
        fv = THD_3dmm_to_dicomm( dset , fv ) ;
        UNLOAD_FVEC3(fv,xv,yv,zv) ;

        for( pbest=-1,qq=0 ; qq < 26 ; qq++ ){ /* loop over neighbors and */
                                               /* find closest mapped pt */

          pp = vmap[(ii+ip[qq][0]) + (jj+ip[qq][1])*nx + (kk+ip[qq][2])*nxy];

          if( pp >= 0 ){
             pp = AGNI_VMAP_UNMASK(pp) ;      /* index of mapped pt */
             xp=xv-ag->nod[pp].x; yp=yv-ag->nod[pp].y; zp=zv-ag->nod[pp].z;
             dd=xp*xp+yp*yp+zp*zp ;           /* dist^2 to mapped pt */
             if( pbest >= 0 ){
                if( dd < dbest ){ pbest = pp ; dbest = dd ; }
             } else {
                pbest = pp ; dbest = dd ;
             }
          }
        }

        /* save closest of the neighbors;
           temporarily as a large negative number,
           so we won't hit it again in this level of expansion */

        if( pbest >= 0 ) vmap[ijk] = -(pbest+ntop) ; /* closest of the neighbors */

    }}} /* end of loop over voxels */

    STATUS(".. masking") ;

    lmask = AGNI_VMAP_LEVMASK(lev) ;   /* 07 Sep 2001: put on a mask */
                                       /* to indicate which level of */
                                       /* indirection this voxel was */

    for( kk=kbot ; kk < ktop ; kk++ ){  /* change all the ones we found  */
     for( jj=jbot ; jj < jtop ; jj++ ){ /* at this level to non-negative */
      for( ii=ibot ; ii < itop ; ii++ ){
        ijk = ii+jj*nx+kk*nxy ;
        if( vmap[ijk] < -1 ) vmap[ijk] = (-vmap[ijk] - ntop) | lmask ;
    }}}

fprintf(stderr,".") ;

   } /* end of loop over lev */

fprintf(stderr,"\n") ;

   RETURN( vmap );
}

/*-------------------------------------------------------------------------*/

void AGNI_get_sname( THD_3dim_dataset *dset )
{
   char *snam ;
   int ii ;

ENTRY("AGNI_get_sname") ;

   if( !ISVALID_DSET(dset) || dset->ag_sname != NULL ) EXRETURN ;

   snam = strdup( DSET_HEADNAME(dset) ) ;
   ii = strlen(snam) ;
   if( ii > 5 ){
      strcpy(snam+ii-4,"SURF") ;
      if( THD_is_file(snam) ){ dset->ag_sname = snam; EXRETURN; }
   }
   free(snam) ; EXRETURN ;
}

/*-------------------------------------------------------------------------*/

void AGNI_load( THD_3dim_dataset *dset )
{

ENTRY("AGNI_load") ;

   if( !ISVALID_DSET(dset)    ||
       dset->ag_sname == NULL || dset->ag_surf != NULL ) EXRETURN ;

   dset->ag_surf = AGNI_read_surface( dset->ag_sname , dset ) ;

   if( dset->ag_surf == NULL ){
      free(dset->ag_sname) ; dset->ag_sname = NULL ; EXRETURN ;
   }

   if( dset->ag_vmap != NULL ) free(dset->ag_vmap) ;

   dset->ag_vmap = AGNI_map_dset_to_surf( dset->ag_surf , dset ) ;

   EXRETURN ;
}

/*--------------------------------------------------------------------------*/

void AGNI_unload( THD_3dim_dataset *dset )
{

ENTRY("AGNI_unload") ;

   if( !ISVALID_DSET(dset) || dset->ag_sname == NULL ) EXRETURN ;

   if( dset->ag_surf != NULL ){
      AGNI_destroy_surface( dset->ag_surf ) ; dset->ag_surf = NULL ;
   }

   if( dset->ag_vmap != NULL ){
      free( dset->ag_vmap ) ; dset->ag_vmap = NULL ;
   }

   EXRETURN ;
}

/*--------------------------------------------------------------------------
   The following routines are used to convert DICOM order coordinates
   (used in AFNI) to SureFit order coordinates -- 25 Oct 2001 - RWCox
----------------------------------------------------------------------------*/

THD_fvec3 THD_dicomm_to_surefit( THD_3dim_dataset *dset , THD_fvec3 fv )
{
   float xx,yy,zz , xbase,ybase,zbase ;
   THD_fvec3 vout ;

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

   vout.xyz[0] = xx - xbase ;
   vout.xyz[1] = yy - ybase ;
   vout.xyz[2] = zz - zbase ; return vout ;
}

/* --------------------------------------------------------------------------*/

THD_fvec3 THD_surefit_to_dicomm( THD_3dim_dataset *dset , THD_fvec3 fv )
{
   float xx,yy,zz , xbase,ybase,zbase ;
   THD_fvec3 vout ;

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

   vout.xyz[0] = xx - xbase ;
   vout.xyz[1] = yy - ybase ;
   vout.xyz[2] = zz + zbase ; return vout ;
}

/*---------------------------------------------------------------------
  Read a <SureFit .../> line into a surface
    ag   = already existing surface (nodes loaded into this)
    lbuf = line containing "<SureFit"
    dset = dataset for SureFit-to-DICOM coordinate conversion
-----------------------------------------------------------------------*/

void AGNI_import_surefit( AGNI_surface *ag, char *lbuf, THD_3dim_dataset *dset )
{
   float xx[NBUF],yy[NBUF],zz[NBUF] ;
   int   pp[NBUF] ;
   int nn , ii , idadd=0 ;
   FILE *sfp ;
   char sname[1024] , *cpt ;
   THD_fvec3 fv ;

ENTRY("AGNI_import_surefit") ;

   /* scan input line for coord=sname, and extract into sname */

   cpt = strstr(lbuf,"coord=") ;
   if( cpt == NULL ){
      fprintf(stderr,"** AGNI: Illegal SureFit: no coord=\n** %s\n",lbuf) ;
      EXRETURN ;
   }
   cpt += 6 ;                                  /* skip coord= */
   if( *cpt == '\"' || *cpt == '\'' ) cpt++ ;  /* skip quote  */
   ii = sscanf(cpt,"%s",sname) ;               /* get sname   */
   if( ii == 0 ){
      fprintf(stderr,"** AGNI: Illegal SureFit: bad coord=\n** %s\n",lbuf) ;
      EXRETURN ;
   }
   ii = strlen(sname) ;
   if( ii == 0 ){
      fprintf(stderr,"** AGNI: Illegal SureFit: bad coord=\n** %s\n",lbuf) ;
      EXRETURN ;
   }
   if( sname[ii-1] == '\'' || sname[ii-1] == '\"' ) sname[ii-1] = '\0' ;
   if( strlen(sname) == 0 ){
      fprintf(stderr,"** AGNI: Illegal SureFit: bad coord=\n** %s\n",lbuf) ;
      EXRETURN ;
   }

   /* add dataset directory name to start of sname? */

   if( sname[0] != '/' ){
      char buf[1024] ;
      sprintf(buf,"%s%s",DSET_DIRNAME(dset),sname) ;
      strcpy(sname,buf) ;
   }

   /* scan line for IDadd=value, and extract into idadd */

   cpt = strstr(lbuf,"IDadd=") ;
   if( cpt != NULL ){
      cpt += 6 ;
      if( *cpt == '\"' || *cpt == '\'' ) cpt++ ;
      ii = sscanf(cpt,"%d",&idadd) ;
      if( ii == 0 || idadd < 0 ){
         fprintf(stderr,"** AGNI: Illegal SureFit: bad IDadd=\n** %s\n",lbuf) ;
         EXRETURN ;
      }
   }

   /* open sname */

fprintf(stderr,"AGNI: Opening SureFit file %s with IDadd=%d\n",sname,idadd) ;

   sfp = fopen( sname , "r" ) ;
   if( sfp == NULL ){
      fprintf(stderr,"** AGNI: Illegal SureFit: can't open file %s\n** %s\n",sname,lbuf) ;
      EXRETURN ;
   }

   nn = 0 ;

   while(1){
      cpt = fgets(sname,1024,sfp) ;  /* read a line */
      if( cpt == NULL ) break ;      /* end of file */

      if( strstr(sname,"BeginHeader") != NULL ){  /* skip SureFit header */
         do{
            cpt = fgets(sname,1024,sfp) ;                /* get next line */
            if( cpt == NULL ){ fclose(sfp); EXRETURN; }  /* bad */
         } while( strstr(sname,"EndHeader") == NULL ) ;
         cpt = fgets(sname,1024,sfp) ;                   /* 1 more line */
         if( cpt == NULL ){ fclose(sfp); EXRETURN; }     /* bad */
         continue ;                                      /* start over */
      }

      ii = sscanf(sname,"%d%f%f%f",pp+nn,xx+nn,yy+nn,zz+nn) ;
      if( ii < 4 ) continue ;   /* skip this line; it's bad */

      /* process value to AFNI-ize it from SureFit */

      pp[nn] += idadd ;
      LOAD_FVEC3(fv,xx[nn],yy[nn],zz[nn]) ;
      fv = THD_surefit_to_dicomm( dset , fv ) ;
      UNLOAD_FVEC3(fv,xx[nn],yy[nn],zz[nn]) ;

      nn++ ;
      if( nn == NBUF ){
         AGNI_add_nodes_ixyz( ag,nn , pp,xx,yy,zz ) ;
         nn = 0 ;
      }
   } /* end of loop over input lines */

   fclose(sfp) ;

   if( nn > 0 ){
      AGNI_add_nodes_ixyz( ag,nn , pp,xx,yy,zz ) ;
   }

   EXRETURN ;
}

/*===================*/
#endif /* ALLOW_AGNI */
