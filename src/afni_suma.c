#include "mrilib.h"
#include "pbardefs.h"

/********************************************************************
 ****** Functions to create and deal with SUMA_surface structs ******
 ********************************************************************/

#define SUMA_EXTEND_NUM 64
#define SUMA_EXTEND_FAC 1.05

/*------------------------------------------------------------------*/
/*! Create an empty surface description.
--------------------------------------------------------------------*/

SUMA_surface * SUMA_create_empty_surface(void)
{
   SUMA_surface *ag ;

ENTRY("SUMA_create_empty_surface") ;

   ag       = (SUMA_surface *) calloc(1,sizeof(SUMA_surface)) ;
   ag->type = SUMA_SURFACE_TYPE ;

   ag->num_ixyz  = ag->num_ijk  = 0 ;
   ag->nall_ixyz = ag->nall_ijk = 1 ;
   ag->ixyz = (SUMA_ixyz *) malloc(sizeof(SUMA_ixyz)) ; /* space for */
   ag->ijk  = (SUMA_ijk *)  malloc(sizeof(SUMA_ijk) ) ; /* 1 of each */
   ag->norm = NULL ;                                  ; /* none of this */

   if( ag->ixyz == NULL || ag->ijk == NULL ){
      fprintf(stderr,"SUMA_create_empty_surface: can't malloc!\n"); EXIT(1);
   }

   ag->idcode[0] = ag->idcode_dset[0] = ag->idcode_ldp[0] =
      ag->label[0] = ag->label_ldp[0] = '\0' ;

   ag->xbot = ag->ybot = ag->zbot =  WAY_BIG ;
   ag->xtop = ag->ytop = ag->ztop = -WAY_BIG ;
   ag->xcen = ag->ycen = ag->zcen = 0.0      ;

   ag->seq = ag->seqbase = ag->sorted = 0 ; /* not sequential; not sorted */

   ag->vv = NULL ;  /* 16 Jun 2003 */
   ag->vn = NULL ;  /* 22 Jan 2004 */

   RETURN( ag ) ;
}

/*------------------------------------------------------------------*/
/*! Throw out some trash (i.e., free the contents of a surface).
--------------------------------------------------------------------*/

void SUMA_destroy_surface( SUMA_surface *ag )
{
ENTRY("SUMA_destroy_surface") ;

   if( ag == NULL ) EXRETURN ;
   if( ag->ixyz != NULL ) free((void *)ag->ixyz) ;
   if( ag->ijk  != NULL ) free((void *)ag->ijk) ;
   if( ag->norm != NULL ) free((void *)ag->norm) ;

   if( ag->vv != NULL ) DESTROY_VVLIST(ag->vv) ;
   if( ag->vn != NULL ) SUMA_destroy_vnlist(ag->vn) ;

   free((void *)ag) ; EXRETURN ;
}

/*------------------------------------------------------------------*/
/* Add a bunch of nodes to a surface.
--------------------------------------------------------------------*/

void SUMA_add_nodes_ixyz( SUMA_surface *ag, int nadd,
                          int *iadd, float *xadd, float *yadd, float *zadd )
{
   int ii , nup ;

ENTRY("SUMA_add_nodes_ixyz") ;

   if( ag == NULL || nadd < 1 ) EXRETURN ;
   if( xadd == NULL || yadd == NULL || zadd == NULL || iadd == NULL ) EXRETURN ;

   nup = ag->num_ixyz + nadd ;

   if( nup >= SUMA_MAX_NODES ){  /* 07 Sep 2001 */
     fprintf(stderr,
             "** SUMA surface can't have more than %d nodes!\n",
             SUMA_MAX_NODES-1 ) ;
     EXRETURN ;
   }

   if( nup > ag->nall_ixyz ){ /* extend length of array */
     ag->nall_ixyz = nup = nup*SUMA_EXTEND_FAC + SUMA_EXTEND_NUM ;
     ag->ixyz = (SUMA_ixyz *) realloc( (void *)ag->ixyz, sizeof(SUMA_ixyz)*nup );
     if( ag->ixyz == NULL ){
       fprintf(stderr,"SUMA_add_nodes_ixyz: can't malloc!\n"); EXIT(1);
     }
   }

   nup = ag->num_ixyz ;

   for( ii=0 ; ii < nadd ; ii++ ){
     ag->ixyz[ii+nup].x  = xadd[ii] ;
     ag->ixyz[ii+nup].y  = yadd[ii] ;
     ag->ixyz[ii+nup].z  = zadd[ii] ;
     ag->ixyz[ii+nup].id = iadd[ii] ;
   }

   ag->num_ixyz += nadd ;

   ag->seq = ag->sorted = 0 ; EXRETURN ;
}

/*--------------------------------------------------------------------
 * Add/replace normals on the given surface.       05 Oct 2004 [rickr]
 *
 * This function requires one normal per node, and that the
 * indices match.
 *--------------------------------------------------------------------
*/
int SUMA_add_norms_xyz( SUMA_surface *ag, int nadd,
                        float *xadd, float *yadd, float *zadd )
{
   int ii ;

ENTRY("SUMA_add_norms_xyz") ;

   if( ag == NULL || nadd < 1 ) RETURN(-1) ;
   if( xadd == NULL || yadd == NULL || zadd == NULL ) RETURN(-1) ;

   if( nadd != ag->num_ixyz ){
     fprintf(stderr, "** SUMA surface has %d nodes but %d normals!\n",
             ag->num_ixyz, nadd ) ;
     RETURN(-1) ;
   }

   /* if norm is NULL, memory is needed */
   if( ag->norm == NULL ){
       ag->norm = (THD_fvec3 *)calloc(nadd, sizeof(THD_fvec3));
       if( ag->norm == NULL ){
           fprintf(stderr,"SUMA_add_norms_xyz: can't malloc!\n"); EXIT(1);
       }
   }

   for( ii=0 ; ii < nadd ; ii++ ){
     ag->norm[ii].xyz[0] = xadd[ii] ;
     ag->norm[ii].xyz[1] = yadd[ii] ;
     ag->norm[ii].xyz[2] = zadd[ii] ;
   }

   RETURN(0) ;
}

/*------------------------------------------------------------------*/
/*! Add 1 pitiful node to a surface.
--------------------------------------------------------------------*/

void SUMA_add_node_ixyz( SUMA_surface *ag , int i,float x,float y,float z )
{
   SUMA_add_nodes_ixyz( ag , 1 , &i,&x,&y,&z ) ;
}

/*------------------------------------------------------------------*/
/*!  Add a bunch of triangles (node id triples) to a surface.
--------------------------------------------------------------------*/

void SUMA_add_triangles( SUMA_surface *ag, int nadd, int *it, int *jt, int *kt )
{
   int ii , nup ;

ENTRY("SUMA_add_triangles") ;

   if( ag == NULL || nadd < 1 ) EXRETURN ;
   if( it == NULL || jt == NULL || kt == NULL ) EXRETURN ;

   nup = ag->num_ijk + nadd ;
   if( nup > ag->nall_ijk ){ /* extend length of array */
     ag->nall_ijk = nup = nup*SUMA_EXTEND_FAC + SUMA_EXTEND_NUM ;
     ag->ijk = (SUMA_ijk *) realloc( (void *)ag->ijk , sizeof(SUMA_ijk)*nup ) ;
     if( ag->ijk == NULL ){
       fprintf(stderr,"SUMA_add_triangles: can't malloc!\n"); EXIT(1);
     }
   }

   nup = ag->num_ijk ;
   for( ii=0 ; ii < nadd ; ii++ ){
     ag->ijk[ii+nup].id = it[ii] ;
     ag->ijk[ii+nup].jd = jt[ii] ;
     ag->ijk[ii+nup].kd = kt[ii] ;
   }

   ag->num_ijk += nadd ; EXRETURN ;
}

/*------------------------------------------------------------------*/
/*! Add 1 pitiful triangle to a surface.
--------------------------------------------------------------------*/

void SUMA_add_triangle( SUMA_surface *ag, int it, int jt, int kt )
{
   SUMA_add_triangles( ag , 1 , &it,&jt,&kt ) ;
}

/*------------------------------------------------------------------*/
/*! Truncate the memory used by the node and triangle arrays back
    to the minimum they need.
--------------------------------------------------------------------*/

void SUMA_truncate_memory( SUMA_surface *ag )
{
   int nn ;

ENTRY("SUMA_truncate_memory") ;

   if( ag == NULL ) EXRETURN ;

   if( ag->num_ixyz < ag->nall_ixyz && ag->num_ixyz > 0 ){
     ag->nall_ixyz = nn = ag->num_ixyz ;
     ag->ixyz = (SUMA_ixyz *) realloc( (void *)ag->ixyz, sizeof(SUMA_ixyz)*nn );
   }

   if( ag->num_ijk < ag->nall_ijk && ag->num_ijk > 0 ){
     ag->nall_ijk = nn = ag->num_ijk ;
     ag->ijk = (SUMA_ijk *) realloc( (void *)ag->ijk , sizeof(SUMA_ijk)*nn ) ;
   }

   EXRETURN ;
}

/*------------------------------------------------------------------
  Generate a function to sort array of SUMA_ixyz-s by their id-s
--------------------------------------------------------------------*/

#undef  STYPE
#define STYPE     SUMA_ixyz          /* name of type to sort     */
#define SLT(a,b)  ((a).id < (b).id)  /* macro to decide on order */
#define SNAME     SUMA_ixyz          /* function qsort_SUMA_ixyz */
#include "cs_sort_template.h"        /* generate the function    */
#undef  STYPE

/*** Can now use qsort_SUMA_ixyz(int,SUMA_ixyz *) ***/

/*------------------------------------------------------------------*/
/*!  Sort the nodes by id-s, and mark if the id-s are sequential.
--------------------------------------------------------------------*/

void SUMA_ixyzsort_surface( SUMA_surface *ag )
{
   int nn , ii , ndup ;
   float xb,yb,zb , xt,yt,zt , xc,yc,zc ;

ENTRY("SUMA_ixyzsort_surface") ;

   if( ag == NULL || ag->num_ixyz < 1 ) EXRETURN ;

   SUMA_truncate_memory( ag ) ;

   nn = ag->num_ixyz ;

   /* check if nodes are already sorted [26 Oct 2001] */

   for( ii=1 ; ii < nn ; ii++ )
     if( ag->ixyz[ii].id <= ag->ixyz[ii-1].id ) break ;

   /* if not in increasing order,
      sort them using the function generated above */

   if( ii < nn ){
     qsort_SUMA_ixyz( nn , ag->ixyz ) ;
   }

   ag->sorted = 1 ;  /* mark as sorted */

   /* check if node id-s are sequential */

   for( ii=1 ; ii < nn ; ii++ )
     if( ag->ixyz[ii].id != ag->ixyz[ii-1].id+1 ) break ;

   /* if we finished that loop all the way,
      mark the nodes as being sequential, and
      store the base of the sequence (id of node #0) */

   if( ii == nn ){
     ag->seq = 1 ; ag->seqbase = ag->ixyz[0].id ;
   }

   /* 07 Sep 2001: check for duplicate node id-s */

   for( ndup=0,ii=1 ; ii < nn ; ii++ )
     if( ag->ixyz[ii].id == ag->ixyz[ii-1].id ) ndup++ ;

   if( ndup > 0 )
     fprintf(stderr,"** SUMA WARNING: %d duplicate surface node id's found!\n",ndup);

   /* find bounding box of all nodes (it's useful on occasion) */

   xb = xt = ag->ixyz[0].x ;
   yb = yt = ag->ixyz[0].y ;
   zb = zt = ag->ixyz[0].z ;
   xc = yc = zc = 0.0 ;
   for( ii=1 ; ii < nn ; ii++ ){
     xc += ag->ixyz[ii].x ;
     yc += ag->ixyz[ii].y ;
     zc += ag->ixyz[ii].z ;

          if( ag->ixyz[ii].x < xb ) xb = ag->ixyz[ii].x ;
     else if( ag->ixyz[ii].x > xt ) xt = ag->ixyz[ii].x ;

          if( ag->ixyz[ii].y < yb ) yb = ag->ixyz[ii].y ;
     else if( ag->ixyz[ii].y > yt ) yt = ag->ixyz[ii].y ;

          if( ag->ixyz[ii].z < zb ) zb = ag->ixyz[ii].z ;
     else if( ag->ixyz[ii].z > zt ) zt = ag->ixyz[ii].z ;
   }

   ag->xbot = xb ; ag->xtop = xt ;
   ag->ybot = yb ; ag->ytop = yt ;
   ag->zbot = zb ; ag->ztop = zt ;

   ag->xcen = xc/nn ; ag->ycen = yc/nn ; ag->zcen = zc/nn ;

   EXRETURN ;
}

/*--------------------------------------------------------------------*/
/*! Find a node id in a surface, and return its index into the node
    array; return -1 if not found.
----------------------------------------------------------------------*/

int SUMA_find_node_id( SUMA_surface *ag , int target )
{
   int nn , ii,jj,kk ;

   if( ag == NULL || ag->num_ixyz < 1 || target < 0 ) return( -1 );

   if( !ag->sorted ) SUMA_ixyzsort_surface( ag ) ;

   if( ag->seq ){  /* node id-s are sequential (the easy case) */
      kk = target - ag->seqbase ;
      if( kk >= 0 && kk < ag->num_ixyz ) return( kk );
      return( -1 );
   }

   /* node id-s are in increasing order, but not sequential;
      so, use binary search to find the node id (if present) */

   ii = 0 ; jj = ag->num_ixyz - 1 ;                 /* search bounds */

        if( target <  ag->ixyz[0].id  ) return( -1 ); /* not present */
   else if( target == ag->ixyz[0].id  ) return( ii ); /* at start!  */

        if( target >  ag->ixyz[jj].id ) return( -1 ); /* not present */
   else if( target == ag->ixyz[jj].id ) return( jj ); /* at end!    */

   while( jj - ii > 1 ){  /* while search bounds not too close */

      kk = (ii+jj) / 2 ;  /* midway between search bounds */

      nn = ag->ixyz[kk].id - target ;
      if( nn == 0 ) return( kk );     /* AHA! */

      if( nn < 0 ) ii = kk ;          /* kk before target => bottom = kk */
      else         jj = kk ;          /* kk after target  => top    = kk */
   }

   return( -1 );
}

/*-------------------------------------------------------------------------*/
/*! Create the voxel-to-node list for this surface/dataset combo.
---------------------------------------------------------------------------*/

SUMA_vnlist * SUMA_make_vnlist( SUMA_surface *ag , THD_3dim_dataset *dset )
{
   int ii,jj,kk , nx,ny,nz , nxy,nxyz , nnode , pp,qq,nn,nvox  ;
   THD_fvec3 fv ;
   THD_ivec3 iv ;
   int *vlist , *nlist , wodsave ;
   SUMA_vnlist *vnlist ;
   float xbot,xtop , ybot,ytop , zbot,ztop ;

ENTRY("SUMA_make_vnlist") ;

   if( ag == NULL || ag->num_ixyz < 1 || !ISVALID_DSET(dset) ) RETURN(NULL) ;

   if( !ag->sorted ) SUMA_ixyzsort_surface( ag ) ;

   /* setup: create arrays for voxel list and node list */

   nx = DSET_NX(dset) ; ny = DSET_NY(dset) ; nz = DSET_NZ(dset) ;
   nxy = nx*ny ; nxyz = nxy*nz ; nnode = ag->num_ixyz ;
   vlist = (int *) malloc(sizeof(int)*nnode) ;
   nlist = (int *) malloc(sizeof(int)*nnode) ;
   if( vlist == NULL || nlist == NULL ){
      fprintf(stderr,"SUMA_make_vnlist: can't malloc!\n"); EXIT(1);
   }

   /* for each node, find which voxel it is in */

   wodsave = dset->wod_flag ; dset->wod_flag = 0 ;

   xbot = DSET_XXMIN(dset) ; xtop = DSET_XXMAX(dset) ;
   ybot = DSET_YYMIN(dset) ; ytop = DSET_YYMAX(dset) ;
   zbot = DSET_ZZMIN(dset) ; ztop = DSET_ZZMAX(dset) ;

   for( nn=pp=0 ; pp < nnode ; pp++ ){
      LOAD_FVEC3( fv , ag->ixyz[pp].x, ag->ixyz[pp].y, ag->ixyz[pp].z ) ;
      fv = THD_dicomm_to_3dmm( dset , fv ) ; /* convert Dicom coords */

      if( fv.xyz[0] < xbot || fv.xyz[0] > xtop ) continue ;
      if( fv.xyz[1] < ybot || fv.xyz[1] > ytop ) continue ;
      if( fv.xyz[2] < zbot || fv.xyz[2] > ztop ) continue ;

      iv = THD_3dmm_to_3dind( dset , fv ) ;  /*   in surface to     */
      UNLOAD_IVEC3( iv , ii,jj,kk ) ;        /*   dataset indexes  */

      nlist[nn] = pp ;                       /* list of nodes */
      vlist[nn] = ii + jj*nx + kk*nxy ;      /* list of voxels */
      nn++ ;
   }

   nnode = nn ; /* number of nodes inside dataset volume */
   if( nnode == 0 ){ free(nlist); free(vlist); RETURN(NULL); }

   dset->wod_flag = wodsave ;

   /* now sort the 2 lists so that vlist is increasing
      (and each nlist still corresponds to its original vlist) */

   qsort_intint( nnode , vlist , nlist ) ;

   /* count how many distinct voxels we found */

   nvox = 1 ; ii = vlist[0] ;
   for( pp=1 ; pp < nnode ; pp++ ){
      if( vlist[pp] != ii ){ nvox++; ii = vlist[pp]; }
   }

   /* now create the output vnlist */

   /* from malloc (no effect now)   12 Feb 2009 [lesstif patrol] */
   vnlist         = (SUMA_vnlist *) calloc( 1, sizeof(SUMA_vnlist) ) ;
   vnlist->nvox   = nvox ;
   vnlist->voxijk = (int *) malloc(sizeof(int) *nvox) ;
   vnlist->numnod = (int *) calloc(sizeof(int) ,nvox) ;
   vnlist->nlist  = (int **)malloc(sizeof(int*)*nvox);
   vnlist->dset   = dset ;

   if( vnlist->voxijk==NULL || vnlist->numnod==NULL || vnlist->nlist==NULL ){
     fprintf(stderr,"SUMA_make_vnlist: can't malloc!\n"); EXIT(1);
   }

   /* now count how many nodes are at each voxel in the list */

   ii = vlist[0] ; qq = nn = 0 ;
   for( pp=1 ; pp < nnode ; pp++ ){
     if( vlist[pp] != ii ){         /* qq..pp-1 are the same */
       vnlist->voxijk[nn] = ii ;
       vnlist->numnod[nn] = jj = pp-qq ;
       vnlist->nlist[nn]  = (int *) malloc(sizeof(int)*jj) ;
       memcpy( vnlist->nlist[nn] , nlist+qq , sizeof(int)*jj ) ;
       ii = vlist[pp] ; nn++ ; qq = pp ;
     }
   }
   vnlist->voxijk[nn] = ii ;
   vnlist->numnod[nn] = jj = pp-qq ;
   vnlist->nlist[nn]  = (int *) malloc(sizeof(int)*jj) ;
   memcpy( vnlist->nlist[nn] , nlist+qq , sizeof(int)*jj ) ;

   /* and we're done! */

   free(nlist) ; free(vlist) ; RETURN( vnlist ) ;
}

/*-------------------------------------------------------------------------*/
/*! Destroy a SUMA_vnlist struct.
---------------------------------------------------------------------------*/

void SUMA_destroy_vnlist( SUMA_vnlist *vnlist )
{
   int ii ;
   if( vnlist == NULL ) return ;
   if( vnlist->voxijk != NULL ) free( vnlist->voxijk ) ;
   if( vnlist->numnod != NULL ) free( vnlist->numnod ) ;
   if( vnlist->nlist  != NULL ){
     for( ii=0 ; ii < vnlist->nvox ; ii++ )
       if( vnlist->nlist[ii] != NULL ) free( vnlist->nlist[ii] ) ;
     free( vnlist->nlist ) ;
   }
   free( vnlist ) ;
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

/****************************************************************************
 ********** AFNI no longer reads surface from files [22 Jan 2004] ***********
 ****************************************************************************/

#undef ALLOW_SURFACE_FILES
#ifdef ALLOW_SURFACE_FILES
/*----------------------------------------------------------*/
/*! Will load this many items (nodes, triangles) at a time. */

#undef  NBUF
#define NBUF 64

/*------------------------------------------------------------------------*/
/*! Read a surface description file that is associated with an AFNI
    dataset.  Return a surface ready to rock-n-roll.
    NULL is returned if something bad happens.
--------------------------------------------------------------------------*/

SUMA_surface * SUMA_read_surface( char *fname , THD_3dim_dataset *dset )
{
   SUMA_surface *ag ;
   FILE *fp ;
   char lbuf[1024] , *cpt ;
   int  do_nod=1 , ii ;
   float xx[NBUF],yy[NBUF],zz[NBUF] ;
   int   pp[NBUF],qq[NBUF],rr[NBUF] , nn ;

   THD_vecmat mv ;
   int have_mv=0 ;

ENTRY("SUMA_read_surface") ;

   if( fname == NULL || fname[0] == '\0' ) RETURN( NULL );

   /*-- open input --*/

   if( strcmp(fname,"-") == 0 ){   /* special case */
      fp = stdin ;
   } else {
      fp = fopen( fname , "r" ) ;
      if( fp == NULL ) RETURN( NULL );
   }

   /*-- initialize surface that we will fill up here */

   ag = SUMA_create_empty_surface() ;

   /*-- set IDCODEs of surface (from filename) and of its dataset --*/

   cpt = UNIQ_hashcode(fname); strcpy(ag->idcode,cpt); free(cpt);

   strcpy( ag->idcode_dset , dset->idcode.str ) ;

   MCW_strncpy( ag->label, THD_trailname(fname,0), 32 ) ;  /* 19 Aug 2002 */

   /*-- read data --*/

   nn = 0 ;
   while(1){
      cpt = fgets(lbuf,1024,fp) ;  /* read a line */
      if( cpt == NULL ) break ;    /* end of file */

      /*-- read a transformation matrix-vector? --*/

      if( strncmp(lbuf,"<MATVEC>",8) == 0 ){  /* 07 Sep 2001 */
         float a11,a12,a13 , v1 ,
               a21,a22,a23 , v2 ,
               a31,a32,a33 , v3  ;

         ii = sscanf( lbuf+8 , "%f%f%f%f%f%f%f%f%f%f%f%f" ,
                      &a11,&a12,&a13 , &v1 ,
                      &a21,&a22,&a23 , &v2 ,
                      &a31,&a32,&a33 , &v3  ) ;

         if( ii < 12 ){
            fprintf(stderr,"** SUMA: Illegal MATVEC in %s\n",fname) ;
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
               SUMA_add_nodes_ixyz( ag,nn , pp,xx,yy,zz ) ;
            else
               SUMA_add_triangles( ag,nn , pp,qq,rr ) ;
            nn = 0 ;
         }

         SUMA_import_surefit( ag , lbuf , dset ) ;
         continue ; /* skip to next input line */

      } /* end of SureFit input */

      /*-- end of node input? --*/

      if( strstr(lbuf,"</NODES>") != NULL ){
         if( do_nod && nn > 0 ){
            SUMA_add_nodes_ixyz( ag,nn , pp,xx,yy,zz ) ;
            nn = 0 ;
         }
#if 1
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
         if( have_mv ){           /* transform coords */
            THD_fvec3 fv , qv ;
            LOAD_FVEC3(fv , xx[nn],yy[nn],zz[nn] ) ;
            qv = VECSUB_MAT( mv.mm , fv , mv.vv ) ;
            UNLOAD_FVEC3( qv , xx[nn],yy[nn],zz[nn] ) ;
         }
         nn++ ;
         if( nn == NBUF ){        /* add nodes in chunks */
            SUMA_add_nodes_ixyz( ag,nn , pp,xx,yy,zz ) ;
            nn = 0 ;
         }

      /*-- process line as a triangle --*/

      } else {                    /* triangles */

         ii = sscanf(lbuf,"%d%d%d",pp+nn,qq+nn,rr+nn) ;
         if( ii < 3 ) continue ;
         nn++ ;
         if( nn == NBUF ){        /* add triangles in chunks */
            SUMA_add_triangles( ag,nn , pp,qq,rr ) ;
            nn = 0 ;
         }
      }
   } /* end of loop over input lines */

   /*-- finish up, eh? --*/

   if( fp != stdin ) fclose(fp) ;
   if( nn > 0 ){                  /* process leftover data lines */
      if( do_nod )
         SUMA_add_nodes_ixyz( ag,nn , pp,xx,yy,zz ) ;
      else
         SUMA_add_triangles( ag,nn , pp,qq,rr ) ;
   }

   /*-- hope we got something --*/

   if( ag->num_ixyz < 1 ){
      SUMA_destroy_surface(ag) ; RETURN(NULL) ;
   }

   /*-- sort the nodes (if needed), et cetera --*/

   SUMA_ixyzsort_surface(ag) ;

   /*-- done --*/

   RETURN( ag );
}

/*-----------------------------------------------------------------------------*/

/*! 26 point 3x3x3 nbhd of {0,0,0}
    (not including the central point itself) */

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
/*! Given a surface and a dataset, compute a voxel-to-node map.
    This is an int array (the return value) the size of a dataset
    brick - one value per voxel.  Each entry "v" is
     - v < 0 means that voxel is not close to a surface node
     - otherwise, the closest node (index in ag->ixyz, NOT id) to the
        voxel is SUMA_VMAP_UNMASK(v)
     - the "level" of that node is SUMA_VMAP_LEVEL(v),
        where level is the number of nbhd expansions outward from the
        voxel that were needed to hit a node (0<=level<=7).
     - The node index (26 bit integer) is stored in bits 0..25 of v
     - The level (3 bit integer) is stored in bits 26..28 of v
     - Bits 29..30 are reserved for future purposes
     - Bit 31 is the sign bit, and so signifies "no node nearby"

    Other notes:
     - The input surface should have been sorted by SUMA_ixyzsort_surface().
        If it wasn't, it will be now.
     - Although this function was crafted to be efficient, it still takes
        a few seconds to run.
     - Don't add nodes to the surface after calling this, since they
        won't be indexed here properly.  Or you could free() the output
        map and re-invoke this function.
---------------------------------------------------------------------------*/

int * SUMA_map_dset_to_surf( SUMA_surface *ag , THD_3dim_dataset *dset )
{
   int *vmap , ii,jj,kk , nx,ny,nz , nxy,nxyz , pp,qq,pbest ;
   THD_fvec3 fv ;
   THD_ivec3 iv ;
   int ibot,jbot,kbot , itop,jtop,ktop , lev , ijk ;
   float xv,yv,zv , dd,dbest=0 , xp,yp,zp ;
   char *elev ; int ltop , ntop , lmask ;

ENTRY("SUMA_map_dset_to_surf") ;

   if( ag == NULL || ag->num_ixyz < 1 || !ISVALID_DSET(dset) ) RETURN( NULL );

   if( !ag->sorted ) SUMA_ixyzsort_surface( ag ) ;

   /* setup: create the output vmap and fill it with -1 */

   nx = DSET_NX(dset) ; ny = DSET_NY(dset) ; nz = DSET_NZ(dset) ;
   nxy = nx*ny ; nxyz = nxy*nz ;
   vmap = (int *) malloc(sizeof(int)*nxyz) ;
   if( vmap == NULL ){
      fprintf(stderr,"SUMA_map_dset_to_surf: can't malloc!\n"); EXIT(1);
   }
   for( ii=0 ; ii < nxyz ; ii++ ) vmap[ii] = -1 ; /* not mapped yet */

   /* put nodes directly into voxels (level 0) */

STATUS("putting nodes into voxels") ;

   for( pp=0 ; pp < ag->num_ixyz ; pp++ ){
      LOAD_FVEC3( fv , ag->ixyz[pp].x, ag->ixyz[pp].y, ag->ixyz[pp].z ) ;
      fv = THD_dicomm_to_3dmm( dset , fv ) ; /* convert Dicom coords */
      iv = THD_3dmm_to_3dind( dset , fv ) ;  /*   in surface to     */
      UNLOAD_IVEC3( iv , ii,jj,kk ) ;        /*   dataset indexes  */
      qq = vmap[ii+jj*nx+kk*nxy] ;           /* previously mapped index? */
      if( qq < 0 ){                          /* not mapped before */
         vmap[ii+jj*nx+kk*nxy] = pp ;        /* index, not id */
      } else {
         LOAD_IVEC3(iv,ii,jj,kk) ;           /* get Dicom coords of voxel */
         fv = THD_3dind_to_3dmm( dset , iv ) ;
         fv = THD_3dmm_to_dicomm( dset , fv ) ;
         UNLOAD_FVEC3(fv,xv,yv,zv) ;         /* voxel is at (xv,yv,zv) */

         /* find if old node in this voxel (#qq)
            is closer to voxel center than current node (#pp) */

         xp=xv-ag->ixyz[qq].x;
         yp=yv-ag->ixyz[qq].y;
         zp=zv-ag->ixyz[qq].z; dd=xp*xp+yp*yp+zp*zp;    /* dist^2 to old node */

         xp=xv-ag->ixyz[pp].x;
         yp=yv-ag->ixyz[pp].y;
         zp=zv-ag->ixyz[pp].z; dbest=xp*xp+yp*yp+zp*zp; /* dist^2 to new node */

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

   /* we will only try to fill voxels inside
      the rectangular subvolume (ibot..itop,jbot..jtop,kbot..ktop) */

   if( ibot > itop ){ ii=ibot ; ibot=itop; itop=ii; }
   if( jbot > jtop ){ jj=jbot ; jbot=jtop; jtop=jj; }
   if( kbot > ktop ){ kk=kbot ; kbot=ktop; ktop=kk; }
   if( ibot < 1 ) ibot = 1 ; if( itop >= nx ) itop = nx-1 ;
   if( jbot < 1 ) jbot = 1 ; if( jtop >= ny ) jtop = ny-1 ;
   if( kbot < 1 ) kbot = 1 ; if( ktop >= nz ) ktop = nz-1 ;

   /* larger than the largest node id */

   ntop = ag->ixyz[ag->num_ixyz-1].id + 100 ;

   /* scan for voxels that are next to those already mapped,
      out to a given level (default max level is 4)         */

   elev = getenv("SUMA_NBHD_LEVEL") ;  /* find level for expanding out */
   if( elev != NULL ){
      char *cpt ;
      ltop = strtol( elev , &cpt , 10 ) ;
      if( ltop < 0 || ltop > 7 || (ltop == 0 && *cpt != '\0') ) ltop = 4 ;
   } else {
      ltop = 4 ;
   }

   /* loop over levels */

   for( lev=1 ; lev <= ltop ; lev++ ){  /* if ltop = 0, won't be executed */

    if(PRINT_TRACING){
     char str[256]; sprintf(str,"expansion level %d",lev); STATUS(str);
    }

    /* loop over voxel 3D indexes */

    for( kk=kbot ; kk < ktop ; kk++ ){
     for( jj=jbot ; jj < jtop ; jj++ ){
      for( ii=ibot ; ii < itop ; ii++ ){

        ijk = ii+jj*nx+kk*nxy ;         /* index into brick array */
        if( vmap[ijk] >= 0 ) continue ; /* already mapped */

        LOAD_IVEC3(iv,ii,jj,kk) ;               /* get Dicom coords */
        fv = THD_3dind_to_3dmm( dset , iv ) ;
        fv = THD_3dmm_to_dicomm( dset , fv ) ;
        UNLOAD_FVEC3(fv,xv,yv,zv) ;             /* coords = (xv,yv,zv) */

        for( pbest=-1,qq=0 ; qq < 26 ; qq++ ){ /* loop over neighbors and */
                                               /* find closest mapped pt */
                                               /* (at a lower level)    */

          /* pp is the volume map at the qq'th neighboring voxel */

          pp = vmap[(ii+ip[qq][0]) + (jj+ip[qq][1])*nx + (kk+ip[qq][2])*nxy];

          if( pp >= 0 ){                      /* if qq'th nbhr is mapped */
             pp = SUMA_VMAP_UNMASK(pp) ;      /* index of mapped node in qq */
             xp=xv-ag->ixyz[pp].x ;           /* coords of mapped node */
             yp=yv-ag->ixyz[pp].y ;
             zp=zv-ag->ixyz[pp].z ;
             dd=xp*xp+yp*yp+zp*zp ;           /* dist^2 to mapped pt */

             /* save pbest = index of mapped node closest to (ii,jj,kk)
                     dbest = dist^2 of pbest to (ii,jj,kk) voxel center */

             if( pbest >= 0 ){
                if( dd < dbest ){ pbest = pp ; dbest = dd ; }
             } else {
                pbest = pp ; dbest = dd ;
             }
          }
        } /* end of loop over 26 neighbors of (ii,jj,kk) voxel */

        /* save closest of the neighbors;
           temporarily as a large negative number,
           so we won't hit it again in this level of expansion */

        if( pbest >= 0 ) vmap[ijk] = -(pbest+ntop) ; /* closest of the neighbors */

    }}} /* end of loop over voxels (ii,jj,kk) */

    STATUS(".. masking") ;

    /* lmask = 3 bit int for level, shifted to bits 26..28 */

    lmask = SUMA_VMAP_LEVMASK(lev) ;   /* 07 Sep 2001: put on a mask */
                                       /* to indicate which level of */
                                       /* indirection this voxel was */

    for( kk=kbot ; kk < ktop ; kk++ ){  /* change all the nodes we found */
     for( jj=jbot ; jj < jtop ; jj++ ){ /* at this level to non-negative */
      for( ii=ibot ; ii < itop ; ii++ ){
        ijk = ii+jj*nx+kk*nxy ;
        if( vmap[ijk] < -1 ) vmap[ijk] = (-vmap[ijk] - ntop) | lmask ;
    }}}

   } /* end of loop over lev */

   RETURN( vmap );
}

/*-------------------------------------------------------------------------*/
/*! Load the surface for this dataset from its file (if any).
---------------------------------------------------------------------------*/

void SUMA_load( THD_3dim_dataset *dset )
{
   int ks ;  /* 14 Aug 2002: surface index */

ENTRY("SUMA_load") ;

   if( !ISVALID_DSET(dset)   ||
       dset->su_num   == 0   ||
       dset->su_sname == NULL  ) EXRETURN ;

   /* 1st time in: allocate arrays to hold surface data */

   if( dset->su_surf == NULL ){
     dset->su_surf   = (SUMA_surface **) calloc(dset->su_num,sizeof(SUMA_surface *));
     dset->su_vmap   = (int **)          calloc(dset->su_num,sizeof(int *)         );
     dset->su_vnlist = (SUMA_vnlist **)  calloc(dset->su_num,sizeof(SUMA_vnlist *) );
   }

   for( ks=0 ; ks < dset->su_num ; ks++ ){       /* loop over surfaces */

     if( dset->su_surf[ks] != NULL ) continue ;  /* already loaded */

     dset->su_surf[ks] = SUMA_read_surface( dset->su_sname[ks] , dset ) ;

     if( dset->su_surf[ks] == NULL ) continue ;

     if( dset->su_vmap[ks] != NULL ) free(dset->su_vmap[ks]) ;
#if 0
     dset->su_vmap[ks] = SUMA_map_dset_to_surf( dset->su_surf , dset ) ;
#else
     dset->su_vmap[ks] = NULL ;
#endif

     if( dset->su_vnlist[ks] != NULL ){
        SUMA_destroy_vnlist( dset->su_vnlist[ks] ) ;
        dset->su_vnlist[ks] = NULL ;
     }
#if 0
     dset->su_vnlist[ks] = SUMA_make_vnlist( dset->su_surf[ks] , dset ) ;
#endif

   }

   EXRETURN ;
}

/*--------------------------------------------------------------------------*/
/*! Free the surface structs for this dataset (if any).
----------------------------------------------------------------------------*/

void SUMA_unload( THD_3dim_dataset *dset )
{
   int ks ;  /* 14 Aug 2002: surface index */

ENTRY("SUMA_unload") ;

   if( !ISVALID_DSET(dset)    ||
       dset->su_num   == 0    ||
       dset->su_sname == NULL ||
       dset->su_surf  == NULL   ) EXRETURN ;

   for( ks=0 ; ks < dset->su_num ; ks++ ){

     if( dset->su_sname[ks] == NULL                   ||
         strstr(dset->su_sname[ks],"++LOCK++") != NULL  ) continue ;

     if( dset->su_surf[ks] != NULL ){
        SUMA_destroy_surface( dset->su_surf[ks] ) ; dset->su_surf[ks] = NULL ;
     }

     if( dset->su_vmap[ks] != NULL ){
       free( dset->su_vmap[ks] ) ; dset->su_vmap[ks] = NULL ;
     }

     if( dset->su_vnlist[ks] != NULL ){
       SUMA_destroy_vnlist( dset->su_vnlist[ks] ) ; dset->su_vnlist[ks] = NULL ;
     }

   }

   EXRETURN ;
}

/*---------------------------------------------------------------------
  Read a <SureFit .../> line into a surface
    ag   = already existing surface (nodes loaded into this)
    lbuf = line containing "<SureFit"
    dset = dataset for SureFit-to-DICOM coordinate conversion
-----------------------------------------------------------------------*/

void SUMA_import_surefit( SUMA_surface *ag, char *lbuf, THD_3dim_dataset *dset )
{
   float xx[NBUF],yy[NBUF],zz[NBUF] ;
   int   pp[NBUF] ;
   int nn , ii , idadd=0 ;
   FILE *sfp ;
   char sname[1024] , *cpt ;
   THD_fvec3 fv ;

ENTRY("SUMA_import_surefit") ;

   /* scan input line for coord=sname, and extract into sname */

   cpt = strstr(lbuf,"coord=") ;
   if( cpt == NULL ){
      fprintf(stderr,"** SUMA: Illegal SureFit: no coord=\n** %s\n",lbuf) ;
      EXRETURN ;
   }
   cpt += 6 ;                                  /* skip coord= */
   if( *cpt == '\"' || *cpt == '\'' ) cpt++ ;  /* skip quote  */
   ii = sscanf(cpt,"%s",sname) ;               /* get sname   */
   if( ii == 0 ){
      fprintf(stderr,"** SUMA: Illegal SureFit: bad coord=\n** %s\n",lbuf) ;
      EXRETURN ;
   }
   ii = strlen(sname) ;
   if( ii == 0 ){
      fprintf(stderr,"** SUMA: Illegal SureFit: bad coord=\n** %s\n",lbuf) ;
      EXRETURN ;
   }
   if( sname[ii-1] == '\'' || sname[ii-1] == '\"' ) sname[ii-1] = '\0' ;
   if( strlen(sname) == 0 ){
      fprintf(stderr,"** SUMA: Illegal SureFit: bad coord=\n** %s\n",lbuf) ;
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
         fprintf(stderr,"** SUMA: Illegal SureFit: bad IDadd=\n** %s\n",lbuf) ;
         EXRETURN ;
      }
   }

   /* open sname */

   sfp = fopen( sname , "r" ) ;
   if( sfp == NULL ){
      fprintf(stderr,"** SUMA: Illegal SureFit: can't open file %s\n** %s\n",sname,lbuf) ;
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
         SUMA_add_nodes_ixyz( ag,nn , pp,xx,yy,zz ) ;
         nn = 0 ;
      }
   } /* end of loop over input lines */

   fclose(sfp) ;

   if( nn > 0 ){
      SUMA_add_nodes_ixyz( ag,nn , pp,xx,yy,zz ) ;
   }

   EXRETURN ;
}

/*-------------------------------------------------------------------------*/
/*! Load the surface name into the dataset struct (derived by replacing
    .HEAD with .SURF).
---------------------------------------------------------------------------*/

void SUMA_get_surfname( THD_3dim_dataset *dset )
{
   char *snam ;
   int ii , ks ;

ENTRY("THD_get_surfname") ;

   if( !ISVALID_DSET(dset) || dset->su_num > 0 ) EXRETURN ;

   snam = strdup( DSET_HEADNAME(dset) ) ;
   ii = strlen(snam) ;
   if( ii > 5 ){
      strcpy(snam+ii-4,"SURF") ;
      if( THD_is_file(snam) ){
        dset->su_num      = 1 ;
        dset->su_sname    = (char **) malloc(sizeof(char *)) ;
        dset->su_sname[0] = snam;
        EXRETURN;
      }
   }
   free(snam) ; EXRETURN ;  /* .SURF file does not exist */
}


#endif  /* ALLOW_SURFACE_FILES */
