#include "mrilib.h" 
#include "suma_objs.h"
#include "gicor.h"
#include "TrackIO.h"
#include "readglob.h"      // need numbers of things for DTI in/out
#include "suma_objs.h"

/* 
   [PT: July 15, 2020] from include "suma_suma.h" -> "suma_objs.h"
*/

static int NI_tract_type = -1;
int get_NI_tract_type(void) {
   if (NI_tract_type == -1) {
      if ((NI_tract_type = 
           NI_rowtype_define( "TAYLOR_TRACT_DATUM", 
                              TAYLOR_TRACT_DATUM_NIML_DEF)) < 0) {
         ERROR_message("Failed to define NIML tract type");
         return(-2);
      }
   }
   return(NI_tract_type);
}

static int tract_verb = 0;
int get_tract_verb(void) { return(tract_verb); }
void set_tract_verb(int v) { tract_verb=v; return; }

/*!

  tracts_buff (TAYLOR_TRACT *) array of N_tractsbuf tracts. 
  free tracts_buff when this
  function returns.
                                 
  grid (THD_3dim_dataset *) grid defining coordinate space.
  Only used at initialization level
*/ 
TAYLOR_BUNDLE *AppCreateBundle(TAYLOR_BUNDLE *tbu, int N_tractsbuf, 
                               TAYLOR_TRACT *tracts_buff)
{
   TAYLOR_BUNDLE *tb=NULL;
   int nn, tinb;
   TAYLOR_TRACT *tt=NULL;
   
   ENTRY("AppCreateBundle");
   
   if (!tbu) {
      tb = (TAYLOR_BUNDLE *)calloc(1,sizeof(TAYLOR_BUNDLE));
      tb->N_allocated = 0;
      tb->N_tracts = 0;
      tb->N_points_private = -1;
      tb->tract_P0_offset_private = NULL;
      tb->bundle_ends = NULL;
   } else {
      tb = tbu;
      tb->N_points_private = -1; /* reset so that this will get recomputed
                            when Bundle_N_points gets called */
   }
   while (N_tractsbuf > tb->N_allocated - tb->N_tracts) {
      tb->N_allocated += 1000;
      tb->tracts = (TAYLOR_TRACT*)realloc(tb->tracts,
                                          tb->N_allocated*sizeof(TAYLOR_TRACT));
      tb->tract_P0_offset_private = (int *)realloc(tb->tract_P0_offset_private,
                                          tb->N_allocated*sizeof(int));;
   }
   
   if (tracts_buff && N_tractsbuf > 0) {
      for (nn=0; nn<N_tractsbuf; ++nn) {
         tinb = nn+tb->N_tracts;
         tt = tb->tracts+tinb; 
         tt->id = tracts_buff[nn].id;
         tt->N_pts3 = tracts_buff[nn].N_pts3;
         tt->pts = (float *)calloc(tt->N_pts3, sizeof(float));
         if (tract_verb > 1 && nn<3) {
            fprintf(stderr,"AppCreateBundle %d , id %d, N_pts %d, pts %p\n",
						  nn, tt->id, TRACT_NPTS(tt), tracts_buff[nn].pts);
         }
         memcpy(tt->pts, tracts_buff[nn].pts, tt->N_pts3*sizeof(float));
         if (tinb == 0) tb->tract_P0_offset_private[tinb] = 0;
         else {
            --tt; /* get previous tract */
            tb->tract_P0_offset_private[tinb] = 
                  tb->tract_P0_offset_private[tinb-1]+tt->N_pts3/3;
         }
      }
      tb->N_tracts += N_tractsbuf;
   } 
   
   RETURN(tb);
}


/*!
  grid (THD_3dim_dataset *) Without the grid structure, coordinates
  will not be in RAI, but in UHU, unholy units.

  this works a bit differently than originally: tracts are not
  delivered in half anymore but instead in full strings; length of
  tract is not just given but the start and ending locations, A and B
  (respectively and inclusively) in the array, because NOT masks might
  trim away part of string for *some* nets but not all; finally,
  tracts themselves will be included a bit differently-- instead of
  having `test-ends' awkwardly separate, they will now be included in
  the array even though they aren't full locations.

*/
TAYLOR_TRACT *Create_Tract_NEW(int ptA, int ptB, float **pts_buff, 
									int id, THD_3dim_dataset *grid)
{
   TAYLOR_TRACT *tt=NULL;
   int kk = 0, ii=0;
   static int nwarn=0;
   float ORIG[3], Ledge[3];

   ENTRY("Create_Tract");
   
   if (grid) {
      if (ORIENT_typestr[grid->daxes->xxorient][0] != 'R' ||
          ORIENT_typestr[grid->daxes->yyorient][0] != 'A' ||
          ORIENT_typestr[grid->daxes->zzorient][0] != 'I' ) {
         ERROR_message("Only expecting RAI grids");
         RETURN(NULL);
      }  
   } else {
      if (!nwarn) {
         WARNING_message("No grid, coordinates in UHU\n"
                         "Further messages muted\n");
         ++nwarn;
      }
   }
   tt = (TAYLOR_TRACT *)calloc(1, sizeof(TAYLOR_TRACT));
   if (tt == NULL) {
      ERROR_message("Failed to allocate tract");
      RETURN(NULL);
   }
   tt->id = id; tt->N_pts3 = (ptB-ptA+1)*3;
   if (!(tt->pts = (float *)calloc(tt->N_pts3, sizeof(float)))) {
      ERROR_message("Failed to allocate pts vector");
      Free_Tracts(tt,1); RETURN(NULL);
   }

   ORIG[0] = DSET_XORG(grid);
   ORIG[1] = DSET_YORG(grid);
   ORIG[2] = DSET_ZORG(grid);
   Ledge[0] = fabs(DSET_DX(grid)); 
   Ledge[1] = fabs(DSET_DY(grid)); 
   Ledge[2] = fabs(DSET_DZ(grid)); 

   kk=0;
   if (pts_buff) { // A and B are inclusive vals
      for (ii=ptA; ii<=ptB; ++ii) { 
         //       tt->pts[kk] = pts_buff[ii][0]+DSET_XORG(grid);++kk;
         //       tt->pts[kk] = pts_buff[ii][1]+DSET_YORG(grid);++kk;
         //       tt->pts[kk] = pts_buff[ii][2]+DSET_ZORG(grid);++kk;
         tt->pts[kk] = pts_buff[ii][0]+ORIG[0]-0.5*Ledge[0];++kk;
         tt->pts[kk] = pts_buff[ii][1]+ORIG[1]-0.5*Ledge[1];++kk;
         tt->pts[kk] = pts_buff[ii][2]+ORIG[2]-0.5*Ledge[2];++kk;
      }
   }

   RETURN(tt);
}


/*!  THIS VERSION JUST KEPT AROUND FOR 3dTrackID, UNTIL THAT IS FULLY
     REMOVED.

     grid (THD_3dim_dataset *) Without the grid structure, coordinates
     will not be in RAI, but in UHU, unholy units.

----> can prob delete now; Nov,2016

TAYLOR_TRACT *Create_Tract(int N_ptsB, float **pts_buffB, 
									int N_ptsF, float **pts_buffF,
									int id, THD_3dim_dataset *grid)
{
   TAYLOR_TRACT *tt=NULL;
   int kk = 0, ii=0;
   static int nwarn=0;
   
   ENTRY("Create_Tract");
   
   if (grid) {
      if (ORIENT_typestr[grid->daxes->xxorient][0] != 'R' ||
          ORIENT_typestr[grid->daxes->yyorient][0] != 'A' ||
          ORIENT_typestr[grid->daxes->zzorient][0] != 'I' ) {
         ERROR_message("Only expecting RAI grids");
         RETURN(NULL);
      }  
   } else {
      if (!nwarn) {
         WARNING_message("No grid, coordinates in UHU\n"
                         "Further messages muted\n");
         ++nwarn;
      }
   }
   tt = (TAYLOR_TRACT *)calloc(1, sizeof(TAYLOR_TRACT));
   if (tt == NULL) {
      ERROR_message("Failed to allocate tract");
      RETURN(NULL);
   }
   tt->id = id; tt->N_pts3 = (N_ptsB+N_ptsF-1)*3;
   if (!(tt->pts = (float *)calloc(tt->N_pts3, sizeof(float)))) {
      ERROR_message("Failed to allocate pts vector");
      Free_Tracts(tt,1); RETURN(NULL);
   }
   kk=0;
   if (pts_buffB) {
      for (ii=(N_ptsB-1); ii>0; --ii) {
         tt->pts[kk] = pts_buffB[ii][0]+DSET_XORG(grid);++kk;
         tt->pts[kk] = pts_buffB[ii][1]+DSET_YORG(grid);++kk;
         tt->pts[kk] = pts_buffB[ii][2]+DSET_ZORG(grid);++kk;
      }
   }
   if (pts_buffF) {
      for (ii=0; ii<N_ptsF; ++ii) {
         tt->pts[kk] = pts_buffF[ii][0]+DSET_XORG(grid);++kk;
         tt->pts[kk] = pts_buffF[ii][1]+DSET_YORG(grid);++kk;
         tt->pts[kk] = pts_buffF[ii][2]+DSET_ZORG(grid);++kk;
      }
   }
   RETURN(tt);
   }*/

TAYLOR_TRACT *Free_Tracts(TAYLOR_TRACT *tt, int n) 
{
   int i;
   
   ENTRY("Free_Tract");
   if (!tt) RETURN(NULL);
   for (i=0; i<n; ++i) {
      if (tt[i].pts) free(tt[i].pts);
   }
   free(tt); 
   RETURN(NULL);
}

TAYLOR_BUNDLE *Free_Bundle(TAYLOR_BUNDLE *tb) 
{
   ENTRY("Free_Bundle");
   
   if (!tb) RETURN(NULL);
   tb->tracts = Free_Tracts(tb->tracts, tb->N_tracts);
   if (tb->tract_P0_offset_private) free(tb->tract_P0_offset_private);
   if (tb->bundle_ends) free(tb->bundle_ends);
   free(tb);
   RETURN(NULL);
}

TAYLOR_NETWORK *Free_Network(TAYLOR_NETWORK *net) 
{
   TAYLOR_BUNDLE *tb=NULL;
   int i;
   
   ENTRY("Free_Network");
   
   if (!net) RETURN(NULL);
   if (net->grid) DSET_delete(net->grid);
   if (net->FA) DSET_delete(net->FA);
   if (net->tbv) {
      for (i=0; i<net->N_tbv; ++i) {
         tb = net->tbv[i];
         if (tb) {
            tb->tracts = Free_Tracts(tb->tracts, tb->N_tracts);
            free(tb);
         }
         net->tbv[i]=NULL;
      }
      free(net->tbv);
   }
   if (net->bundle_tags) free(net->bundle_tags);
   if (net->bundle_alt_tags) free(net->bundle_alt_tags);
   
   free(net);
   
   RETURN(NULL);
}


void Show_Taylor_Tract(TAYLOR_TRACT *tt, FILE *out, int show_maxu) 
{
   int show_max;
   int ii=0;
   
   ENTRY("Show_Taylor_Tract");
   if (!out) out = stderr;
   if (!tt) {
      fprintf(out,"NULL tt"); 
      EXRETURN;
   }
   fprintf(out,"  track id %d, Npts=%d\n", tt->id, TRACT_NPTS(tt));
   if (show_maxu < 0) show_max = TRACT_NPTS(tt);
   else if (show_maxu == 0) show_max = (TRACT_NPTS(tt) < 5) ? TRACT_NPTS(tt) : 5;
   else show_max = show_maxu;
   for (ii=0; ii<show_max; ++ii) {
      fprintf(out, "   %f %f %f\n", 
				  tt->pts[3*ii], tt->pts[3*ii+1],tt->pts[3*ii+2]);
   }  
   EXRETURN;
}

void Show_Taylor_Bundle(TAYLOR_BUNDLE *tb, FILE *out, int show_maxu) 
{
   int show_max;
   int ii=0;
   ENTRY("Show_Taylor_Bundle");
   if (!out) out = stderr;
   if (!tb) {
      fprintf(out,"NULL tb"); 
      EXRETURN;
   }
   fprintf(out,"  Bundle has %d tracts, Ends %s\n", 
               tb->N_tracts, tb->bundle_ends ? tb->bundle_ends:"NULL");
   if ((show_maxu < 0) || (tb->N_tracts < show_maxu)) show_max = tb->N_tracts;
   else if (show_maxu == 0) show_max = (tb->N_tracts < 5) ? tb->N_tracts : 5;  
   else show_max = show_maxu;
   
   for (ii=0; ii<show_max; ++ii) {
      Show_Taylor_Tract(tb->tracts+ii, out, show_maxu);
   }  
   EXRETURN;
}

void Show_Taylor_Network(TAYLOR_NETWORK *net, FILE *out, 
                         int show_maxu, int show_maxub) 
{
   TAYLOR_BUNDLE *tb=NULL;
   int show_max;
   int ii=0;
   
   ENTRY("Show_Taylor_Network");
   if (!out) out = stderr;
   if (!net) {
      fprintf(out,"NULL net"); 
      EXRETURN;
   }
   fprintf(out,"  Network has %d bundles\n", net->N_tbv);
   if (show_maxu < 0) show_max = net->N_tbv;
   else if (show_maxu == 0) show_max = (net->N_tbv < 5) ? net->N_tbv : 5;  
   else show_max = show_maxu;
   
   for (ii=0; ii<show_max; ++ii)
      Show_Taylor_Bundle(net->tbv[ii], out, show_maxub);
     
   EXRETURN;
}

float Tract_Length(TAYLOR_TRACT *tt) 
{
   float l = -1.0, dx, dy, dz;
   int i, N, i13, i03;
   if (!tt) return(l);
   N = tt->N_pts3/3;
   l = 0.0;
   for (i=1; i<N; ++i) {
      i13 = 3*i; i03 = i13-3;
      dx = tt->pts[i13  ]-tt->pts[i03  ];
      dy = tt->pts[i13+1]-tt->pts[i03+1];
      dz = tt->pts[i13+2]-tt->pts[i03+2];
      l += sqrt(dx*dx+dy*dy+dz*dz);
   }
   return(l);
}

int Bundle_N_points(TAYLOR_BUNDLE *bun, byte recalc)
{
   int it, nn;
   
   if (!bun) return(-1);
   if (!recalc && bun->N_points_private > 0) return(bun->N_points_private);
   for (it=0, nn=0; it<bun->N_tracts; ++it) {
      nn += bun->tracts[it].N_pts3;
   }
   nn /= 3;
   bun->N_points_private = nn;
   return(nn);
}

int Network_N_points(TAYLOR_NETWORK *net, byte recalc)
{
   int nb, ib=0, nn=-1, it;
   
   if (!net) return(-1);
   if (!recalc && net->N_points_private > 0) return(net->N_points_private);
   nn = 0;
   for (ib=0; ib<net->N_tbv; ++ib) {
      if (net->tbv[ib]) {
         nb = 0;
         for (it=0; it<net->tbv[ib]->N_tracts; ++it) {
            nb += net->tbv[ib]->tracts[it].N_pts3;
         }
         net->tbv[ib]->N_points_private = nb/3;
         nn += nb;
      } 
   }
   nn /= 3;
   net->N_points_private = nn;
   return(nn);
}

int Network_N_tracts(TAYLOR_NETWORK *net, byte recalc)
{
   int nb, ib=0, nn=-1, it;
   
   if (!net) return(-1);
   if (!recalc && net->N_tracts_private > 0) return(net->N_tracts_private);
   nn = 0;
   for (ib=0; ib<net->N_tbv; ++ib) {
      if (net->tbv[ib]) {
         nn += net->tbv[ib]->N_tracts;
      } 
   }
   net->N_tracts_private = nn;
   return(nn);
}

int Network_Max_tract_length(TAYLOR_NETWORK *net, byte recalc,
                             int *t, int *b)
{
   int ib, it;
   
   if (!net) return(-1);
   if (!recalc && net->Longest_tract_length_private > 0) {
      if (t) *t = net->Longest_tract_index_in_bundle_private;
      if (b) *b = net->Longest_tract_bundle_index_in_network_private;
      return(net->Longest_tract_length_private);
   }
   net->Longest_tract_length_private = 0;
   for (ib=0; ib<net->N_tbv; ++ib) {
      for (it=0; it<net->tbv[ib]->N_tracts; ++it) {
         if (net->tbv[ib]->tracts[it].N_pts3 > 
             net->Longest_tract_length_private) {
            net->Longest_tract_length_private = net->tbv[ib]->tracts[it].N_pts3;
            net->Longest_tract_index_in_bundle_private = it;
            net->Longest_tract_bundle_index_in_network_private = ib;
         }
      }
   }
   net->Longest_tract_length_private /= 3;
   
   if (t) *t = net->Longest_tract_index_in_bundle_private;
   if (b) *b = net->Longest_tract_bundle_index_in_network_private;
   return(net->Longest_tract_length_private);
}

int Network_N_bundles(TAYLOR_NETWORK *net)
{
   if (!net) return(-1);
   return(net->N_tbv);
}

int Network_PTB_to_1P(TAYLOR_NETWORK *net, int p, int t, int b)
{
   int PP, it, ip, ib;
   
   ENTRY("Network_PTB_to_1P");
   
   if (!net || p<0 || t<0 || b<0) RETURN(-1);
   
   #if 0
   fprintf(stderr,"P %d, T %d, B %d, \n"
                  "N_bundlesNet=%d, N_tractsBundle=%d, N_pointsTract %d\n",
                  p, t, b, b<net->N_tbv ? net->N_tbv:-1,
           (b<net->N_tbv) ? net->tbv[b]->N_tracts:-1,
           (b<net->N_tbv && t<net->tbv[b]->N_tracts) ? 
                                          net->tbv[b]->tracts[t].N_pts3/3 : -1);
   #endif
   
   if (b>=net->N_tbv) RETURN(-1);
   
   if (t>=net->tbv[b]->N_tracts) RETURN(-1);
   
   if ((3*p)>=net->tbv[b]->tracts[t].N_pts3) RETURN(-1);
   
   PP = 0;
   for (ib=0; ib<b; ++ib) {
      PP += Bundle_N_points(net->tbv[ib], 0);
   }
   if (!net->tbv[b]->tract_P0_offset_private) {
      for (it=0; it<t; ++it) {
         PP += (net->tbv[b]->tracts[it].N_pts3/3);
      }
   } else {
      /* use the precomputed offsets */
      if (t > 0) PP += net->tbv[b]->tract_P0_offset_private[t];
   }
   PP += p;
   
   RETURN(PP);
}

int Network_TB_to_1T(TAYLOR_NETWORK *net, int t, int b)
{
   int it, ib, l1=0;
   
   ENTRY("Network_TB_to_1T");
   
   if (!net || b<0 || t<0) RETURN(-1);
   
   #if 0
   fprintf(stderr,"T %d, B %d, \n"
                  "N_bundlesNet=%d, N_tractsBundle=%d, N_pointsTract %d\n",
                  t, b, b<net->N_tbv ? net->N_tbv:-1,
           (b<net->N_tbv) ? net->tbv[b]->N_tracts:-1,
           (b<net->N_tbv && t<net->tbv[b]->N_tracts) ? 
                                          net->tbv[b]->tracts[t].N_pts3/3 : -1);
   #endif
   
   if (b>=net->N_tbv) RETURN(-1);
   
   if (t>=net->tbv[b]->N_tracts) RETURN(-1);
      
   l1 = 0;
   for (ib=0; ib<b; ++ib) {
      l1 += net->tbv[ib]->N_tracts;
   }
   l1 += t;
   
   RETURN(l1);
}

int Network_1T_to_TB(TAYLOR_NETWORK *net, int TT, int *t, int *b, 
                     int *PP0, int *PP1)
{
   int ib;
   
   ENTRY("Network_1T_to_TB");
   
   if (!net || TT < 0) RETURN(-1);
   
   ib = 0;
   while (ib < net->N_tbv && net->tbv[ib]->N_tracts <= TT) {
      TT -= net->tbv[ib]->N_tracts;
      ++ib;
   }
   if (ib >= net->N_tbv) RETURN(-1);
   
   /* We are in bundle ib, tract TT within ib */
   if (b) *b = ib; if (t) *t = TT;
   
   /* what is the 1st and last point (indexed into the network) of that tract? */
   if (PP0) {
      #if 1
      *PP0 = Network_PTB_to_1P(net, 0, TT, ib);    
      #else /* No speed up, don't bother */
      int kb;
      for (kb = 0, *PP0 =0; kb< ib; ++kb) 
         *PP0 += Bundle_N_points(net->tbv[ib], 0);
      for (kb=0; kb<TT; ++kb) *PP0 += net->tbv[ib]->tracts[kb].N_pts3/3;
      #endif
      if (PP1) *PP1 = *PP0+(net->tbv[ib]->tracts[TT].N_pts3/3 - 1);
   }
   RETURN(1);
}

int Network_1B_to_1P(TAYLOR_NETWORK *net, int BB, int *PP1)
{
   int ib, PP0;
   
   ENTRY("Network_1B_to_1P");
   
   if (!net || BB < 0 || BB >= net->N_tbv) RETURN(-1);
   
   ib = PP0 = 0;
   while (ib < BB) {
      PP0 += Bundle_N_points(net->tbv[ib], 0);
      ++ib;
   }
   
   if (PP1) *PP1 = PP0 + Bundle_N_points(net->tbv[BB], 0)-1;
   
   RETURN(PP0);
}

/* Go from point index into the whole network to 
 (bundle in network, tract in bundle, point in tract)
 and if l1u is not NULL, return the index into the whole
 network of the tract containing the point index.
 The latter index is useful for retrieving data stored
 per tract, rather than per point (SUMA_LEV1_DAT). */
int Network_1P_to_PTB(TAYLOR_NETWORK *net, int PP, 
                      int *p, int *t, int *b, int *l1u)
{
   int ib, bnp, it, tnp, l1;
   
   ENTRY("Network_1P_to_PTB");
   
   if (!net || PP<0) RETURN(-1);

   ib = l1 = 0;
   while (ib < net->N_tbv && ((bnp = Bundle_N_points(net->tbv[ib], 0)) <= PP)) {
      PP -= bnp;
      l1 += net->tbv[ib]->N_tracts;
      ++ib;
   }
   if (ib >= net->N_tbv) RETURN(-1);
   
   /* We're in bundle ib, get tract in question */
   it = 0;
   while (it < net->tbv[ib]->N_tracts && 
          ((tnp = net->tbv[ib]->tracts[it].N_pts3/3) <= PP)) {
      PP -= tnp;
      ++it;
   } 
   if (it >= net->tbv[ib]->N_tracts) RETURN(-1);
   l1 += it; /* Tract index (into whole network) */
   
   *p = PP;
   *t = it;
   *b = ib;
   
   if (l1u) *l1u= l1;
   
   RETURN(1);   
}

NI_element *Tract_2_NIel(TAYLOR_TRACT *tt)
{
   NI_element *nel=NULL;
   char colabs[1024]={""};
   
   ENTRY("Tract_2_NIel");
   
   if (!tt || TRACT_NPTS(tt) < 0) RETURN(nel);
   
   nel = NI_new_data_element("tract", TRACT_NPTS(tt));
   NI_SETA_INT(nel, "id", tt->id);
   
   if (tt->pts) {
      strncat(colabs, "x;", (1023-strlen(colabs))*sizeof(char));
      NI_add_column_stride(nel, NI_FLOAT, tt->pts  , 3);
      strncat(colabs, "y;", (1023-strlen(colabs))*sizeof(char));
      NI_add_column_stride(nel, NI_FLOAT, tt->pts+1, 3);
      strncat(colabs, "z;", (1023-strlen(colabs))*sizeof(char));
      NI_add_column_stride(nel, NI_FLOAT, tt->pts+2, 3);
   }
   
   NI_set_attribute(nel,"Column_Labels", colabs);
   RETURN(nel);
}

NI_element *Tracts_2_NIel(TAYLOR_TRACT *tt, int N_tt)
{
   NI_element *nel=NULL;
   
   ENTRY("Tracts_2_NIel");
   
   if (!tt || !N_tt) RETURN(nel);
   
   nel = NI_new_data_element("tracts", N_tt);
   NI_add_column( nel , get_NI_tract_type(), tt );
   
   NI_set_attribute(nel,"Column_Labels", "TaylorTract");
   RETURN(nel);
}

TAYLOR_TRACT *NIel_2_Tracts(NI_element *nel, int *N_tracts)
{
   TAYLOR_TRACT *tt = NULL, *ttn=NULL;
   float *fv0=NULL, *fv1=NULL, *fv2=NULL;
   int ii=0, kk=0, nn=0;
   
   ENTRY("NIel_2_Tracts");
   
   *N_tracts = 0;
   if (!nel) RETURN(tt); 
   
   if (!strcmp(nel->name,"tract")) {
      *N_tracts = 1;
      tt = (TAYLOR_TRACT *)calloc(*N_tracts,sizeof(TAYLOR_TRACT));
      NI_GETA_INT(nel, "id", tt->id);
      tt->N_pts3 = 3*nel->vec_len;
      if (nel->vec_num >= 3) {
         if (!(tt->pts = (float*)calloc(3*nel->vec_len, sizeof(float)))) {
            ERROR_message("Failed to allocate");
            Free_Tracts(tt,*N_tracts); RETURN(NULL);
         }
         fv0 = (float*)nel->vec[0]; 
         fv1 = (float*)nel->vec[1]; 
         fv2 = (float*)nel->vec[2]; 
         kk=0;
         for (ii=0; ii<TRACT_NPTS(tt); ++ii) {
            tt->pts[kk] = fv0[ii]; ++kk;
            tt->pts[kk] = fv1[ii]; ++kk;
            tt->pts[kk] = fv2[ii]; ++kk;
         }
      }
   } else if (!strcmp(nel->name,"tracts")) {
      if (nel->vec_typ[0] != get_NI_tract_type()) {
         ERROR_message("Bad vec_type, have %d, expected %d",
							  nel->vec_typ[0], get_NI_tract_type());
         RETURN(NULL);
      }
      *N_tracts = nel->vec_len;
      tt = (TAYLOR_TRACT *)calloc(*N_tracts,sizeof(TAYLOR_TRACT));
      for (nn=0; nn<*N_tracts; ++nn) {
         ttn = (TAYLOR_TRACT *)(nel->vec[0])+nn;
         tt[nn].id = ttn->id;
         tt[nn].N_pts3 = ttn->N_pts3;
         tt[nn].pts = (float *)calloc(ttn->N_pts3, sizeof(float));
         if (tract_verb && nn<3) {
            fprintf(stderr,"NIel_2_Tracts %d , id %d, N_pts %d, pts %p\n",
						  nn, ttn->id, TRACT_NPTS(ttn), ttn->pts);
         }
         memcpy(tt[nn].pts, ttn->pts, ttn->N_pts3*sizeof(float));
      }       
   }
   RETURN(tt);
}

NI_group *Network_2_NIgr(TAYLOR_NETWORK *net, int mode)
{
   NI_element *nel=NULL;
   NI_group *ngr=NULL, *ngrgrid=NULL, *ngrfa=NULL;
   TAYLOR_BUNDLE *tb=NULL;
   int ii=0, N_All_tracts, ei, ei_alt, bb;
   
   ENTRY("Network_2_NIgr");
   
   if ( !net || !net->tbv || net->N_tbv < 1) RETURN(ngr);
   
   ngr = NI_new_group_element(); NI_rename_group(ngr,"network");
   for (N_All_tracts=0, bb=0; bb<net->N_tbv; ++bb) {
      if ((tb = net->tbv[bb])) {
         N_All_tracts += tb->N_tracts;
      }
   }
   NI_SETA_INT(ngr, "N_tracts", N_All_tracts);
   for (bb=0; bb<net->N_tbv; ++bb) {
      if ((tb = net->tbv[bb])) {
         if (net->bundle_tags) ei = net->bundle_tags[bb];
         else ei = bb;
         if (net->bundle_alt_tags) ei_alt = net->bundle_alt_tags[bb];
         else ei_alt = -1;
         if (tb->tracts) {
            if (mode == 0) { /* slow, handy */
               for (ii=0; ii<tb->N_tracts; ++ii) {
                  nel = Tract_2_NIel(tb->tracts+ii);
                  NI_add_to_group(ngr, nel);
               }
            } else if (mode == 1) { /* fast */
               nel = Tracts_2_NIel(tb->tracts, tb->N_tracts);
               NI_SET_INT(nel,"Bundle_Tag", ei);
               if (ei_alt >= 0) NI_SET_INT(nel,"Bundle_Alt_Tag", ei_alt);
               if (tb->bundle_ends) 
                  NI_SET_STR(nel,"Bundle_Ends", tb->bundle_ends);
               NI_add_to_group(ngr, nel);
            }
         }

      }
   }
   if (net->grid) {
      ngrgrid = THD_dataset_to_niml(net->grid);
      NI_set_attribute(ngrgrid,"bundle_aux_dset","grid");
      NI_add_to_group(ngr, ngrgrid);
      if (net->atlas_space)
         NI_set_attribute(ngr,"atlas_space", net->atlas_space);
   }
   if (net->FA) {
      ngrfa = THD_dataset_to_niml(net->FA);
      NI_set_attribute(ngrfa,"bundle_aux_dset","FA");
      NI_add_to_group(ngr, ngrfa);
   }
   
      
   
   RETURN(ngr);
}

NI_group *Network_link(char *filename)
{
   NI_group *ngr=NULL;
   char *fext = NULL;
   
   ENTRY("Network_link");
   
   if ( !filename) RETURN(ngr);
   fext = SUMA_Extension(filename, ".niml.tract", NOPE);
   ngr = NI_new_group_element(); NI_rename_group(ngr,"network_link");
   NI_set_attribute(ngr, "network_file", fext);
   SUMA_free(fext);
   
   RETURN(ngr);
}


TAYLOR_NETWORK *NIgr_2_Network(NI_group *ngr) 
{
   TAYLOR_NETWORK *net=NULL;
   TAYLOR_BUNDLE *tbb=NULL; 
   TAYLOR_TRACT *tt=NULL;
   NI_element *nel=NULL;
   int ip=0, N_tracts=0, ei=0;
   char *bad=NULL, *sbuf=NULL;
   char tb_ends[128];
   
   ENTRY("NIgr_2_Network");

   if (!ngr) RETURN(net);
   if (!strcmp(ngr->name,"bundle") ||/* old style */
       !strcmp(ngr->name,"network") ) { 
      net = (TAYLOR_NETWORK *)calloc(1,sizeof(TAYLOR_NETWORK));
      net->N_points_private = -1;
      net->N_tracts_private = -1;
      tbb = (TAYLOR_BUNDLE *)calloc(1,sizeof(TAYLOR_BUNDLE));
      tbb->N_points_private = -1;
      for( ip=0 ; ip < ngr->part_num ; ip++ ){ 
         switch( ngr->part_typ[ip] ){
			case NI_GROUP_TYPE:
				if (!(bad = NI_get_attribute(ngr,"bundle_aux_dset"))) {
					WARNING_message("Got unknown group in here! Plodding along");
				}
				if (!strcmp(bad,"grid")) {
					net->grid = THD_niml_to_dataset((NI_group*)ngr->part[ip], 0);
				} else if (!strcmp(bad,"FA")) {
					net->FA = THD_niml_to_dataset((NI_group*)ngr->part[ip], 0);
				} else {
					WARNING_message("Not ready to feel the love for %s\n", bad);
				}  
				if ((sbuf = NI_get_attribute((NI_group*)ngr->part[ip]
													  ,"atlas_space"))) {
					snprintf(net->atlas_space,64*sizeof(char),"%s",sbuf);
				} else {
					snprintf(net->atlas_space,64*sizeof(char),"UNKNOWN");
				}
				break ;
			case NI_ELEMENT_TYPE:
				nel = (NI_element *)ngr->part[ip] ;
				if (!strcmp(nel->name,"tract") || !strcmp(nel->name,"tracts")) {
					if ((tt = NIel_2_Tracts(nel, &N_tracts))) {
						char *be=NULL;
                  tbb = AppCreateBundle(tbb, N_tracts, tt); 
						tt = Free_Tracts(tt, N_tracts);
                  NI_GET_INT(nel,"Bundle_Tag",ei);
                  if (!NI_GOT) ei = -1;
                  if ((be = NI_get_attribute(nel,"Bundle_Ends"))) {
                     net = AppAddBundleToNetwork(net, &tbb, ei, -1, NULL,be);
                  } else {
                     // Sept 2014
                     snprintf( tb_ends, 128, "%03d<->%s", ei,"-1");
                     net = AppAddBundleToNetwork(net, &tbb, ei, -1, NULL, 
                                                 tb_ends);
					   }
               } else {
						WARNING_message("Failed to interpret nel tract,"
											 " ignoring.\n");
					}
				} else {
					WARNING_message("Don't know about nel %s\n", nel->name);
				}
				break;
			default:
				ERROR_message("Don't know what to make of this "
								  "group element, ignoring.");
				break;
         }
      }
   } 
   RETURN(net);
}

TAYLOR_NETWORK *AppAddBundleToNetwork(TAYLOR_NETWORK *network, 
                                      TAYLOR_BUNDLE **tb, int tag, int alt_tag,
                                      THD_3dim_dataset *grid, char *EleName)
{
   TAYLOR_NETWORK *net=NULL;
   
   ENTRY("AppAddBundleToNetwork");
   
   if (!tb) RETURN(net);
   
   if (!network) {
      net = (TAYLOR_NETWORK *)calloc(1,sizeof(TAYLOR_NETWORK));
      net->N_allocated = -1;
      net->N_points_private = -1;
      if (grid) {
         snprintf(net->atlas_space,64*sizeof(char),"%s", grid->atlas_space);
      } else {
         snprintf(net->atlas_space,64*sizeof(char),"UNKNOWN");
      }      
   } else { 
      net = network;
      net->N_points_private = -1; /* uninitialize so that it gets reset later */
   }
   
   if (net->N_allocated <= 0 || net->N_tbv >= net->N_allocated) {
      net->N_allocated += 100;
      net->tbv = (TAYLOR_BUNDLE **)realloc( net->tbv,
                           net->N_allocated*sizeof(TAYLOR_BUNDLE *));
      net->bundle_tags = (int *)realloc(net->bundle_tags,
                                     sizeof(int)*net->N_allocated);
      net->bundle_alt_tags = (int *)realloc(net->bundle_alt_tags,
                                     sizeof(int)*net->N_allocated);
   }
   
   /* PT: Let's chat about the location of bundle_ends and tags soon */
   if (EleName) (*tb)->bundle_ends = strdup(EleName);
   net->tbv[net->N_tbv] = *tb; *tb = NULL;
   net->bundle_tags[net->N_tbv] = tag;
   net->bundle_alt_tags[net->N_tbv] = alt_tag;
   ++net->N_tbv;
   
   RETURN(net);
}

int Write_NI_Network(NI_group *ngr, char *name, char *mode) 
{
   char *nameout=NULL;
   NI_stream ns;
   
   ENTRY("Write_NI_Network");
   
   if (!mode) mode = "NI_fast_binary";
   
   /* be sure to init for tract datum */
   if (get_NI_tract_type() < 0) {
      ERROR_message("Misere!"); 
      RETURN(0);
   }
   if (!name) name = "no_name";

   nameout = (char *)calloc(strlen(name)+35, sizeof(char));
   strcpy(nameout, "file:");
   strcat(nameout,name);
   nameout = without_afni_filename_extension(nameout);
   strcat(nameout,".niml.tract");

   ns = NI_stream_open(nameout, "w");
   if (!ns) {
      ERROR_message("Failed to open NI stream %s for writing.", nameout);
      RETURN(0);
   }
   
   if (tract_verb) {
      fprintf(stderr,"About to write %s in mode %s...", nameout, mode);
   }
   if (strcasestr(mode,"text")) {
      NI_write_element( ns , ngr , NI_TEXT_MODE ) ;
   } else {
      NI_write_element( ns , ngr , NI_BINARY_MODE ) ;
   }
   if (tract_verb) {
      fprintf(stderr,"  Done.\n");
   }
   NI_stream_close(ns); ns=NULL;
   free(nameout);

   RETURN(1);
}

int Write_Bundle(TAYLOR_BUNDLE *tb, char *name, char *mode)
{
   TAYLOR_NETWORK *net=NULL;
   int rval=0;
   
   ENTRY("Write_Bundle");
   
   if (!name) name = "no_name_jack";
   if (!tb) RETURN(0);

   net = (TAYLOR_NETWORK *)calloc(1,sizeof(TAYLOR_NETWORK));
   net->tbv = (TAYLOR_BUNDLE**)calloc(1,sizeof(TAYLOR_BUNDLE*));
   net->bundle_tags = (int *)calloc(1,sizeof(int));
   net->bundle_alt_tags = (int *)calloc(1,sizeof(int));
   net->tbv[0]=tb;
   net->bundle_tags[0]=-1;
   net->bundle_alt_tags[0]=-1;
   net->N_tbv=1;
   
   rval = Write_Network(net, name, mode);
   
   net->tbv[0]=0; net->N_tbv=0;
   Free_Network(net);
   RETURN(rval);
}

int Write_Network(TAYLOR_NETWORK *net, 
                  char *name, char *mode)
{
   NI_group *ngr=NULL;
   int rval=0;
   
   ENTRY("Write_Network");
   if (!name) name = "no_name_jack";
   if (!net) RETURN(0);
   
   if (!mode) mode = "NI_fast";
   if (net->N_tbv > 1 && !strcasestr(mode,"NI_fast")) {
      ERROR_message("Cannot write more than one bundle in slow mode");
      RETURN(0);
   }
   if (strcasestr(mode,"NI_fast")) {
      ngr = Network_2_NIgr(net, 1);
   } else if (strcasestr(mode,"NI_slow")) {
      ngr = Network_2_NIgr(net, 0);
   } else {
      ERROR_message("Stop making bad choices! %s\n",mode);
      RETURN(0);
   }
   
   rval = Write_NI_Network(ngr, name, mode);
   NI_free_element(ngr); ngr=NULL;
   
   RETURN(rval);
} 

NI_group * Read_NI_Network(char *name)
{
   
   NI_stream ns;
   NI_group *ngr=NULL;
   char *nameout=NULL;
   
   ENTRY("Read_NI_Network");
   
   /* be sure to init for tract datum */
   if (get_NI_tract_type() < 0) {
      ERROR_message("Misere!"); 
      RETURN(ngr);
   }
   
   if (!name) RETURN(ngr);
   
   if (strcmp(name,"file:")) { /* have regular file name */
      if (THD_is_file(name)) {
         nameout = (char *)calloc(strlen(name)+35, sizeof(char));
         sprintf(nameout, "file:%s",name);
      } else {
         nameout = (char *)calloc(strlen(name)+35, sizeof(char));
         name = without_afni_filename_extension(name);
         sprintf(nameout,"%s.niml.tract", name);
         if (THD_is_file(nameout)) {
            sprintf(nameout, "file:%s.niml.tract", name);
         } else {
            ERROR_message("Cannot find %s\n", name);
            RETURN(ngr);
         }
      }
   }
   
   ns = NI_stream_open(nameout,"r");
   if (!ns) RETURN(ngr);
   if (get_tract_verb()) fprintf(stderr,"About to read %s ...", nameout);
   ngr = NI_read_element( ns, 1) ;
   if (get_tract_verb()) fprintf(stderr,"  Done.\n");

   NI_stream_close(ns);
   RETURN(ngr);
}

TAYLOR_NETWORK * Read_Network(char *name) 
{
   NI_group *ngr=NULL;
   TAYLOR_NETWORK *net=NULL;
   
   ENTRY("Read_Network");
   
   if (!name) RETURN(net);
   
   if (!(ngr = Read_NI_Network(name))) {
      ERROR_message("Failed to read NI_Bundle %s\n", name);
      RETURN(net);
   }
   
   if (!(net = NIgr_2_Network(ngr))) {
      ERROR_message("Failed to turn group element to bundle %s\n", name);
      NI_free_element(ngr); ngr = NULL;
      RETURN(net);
   }
 
   NI_free_element(ngr); ngr = NULL;
   
   RETURN(net);
}

int NI_getTractAlgOpts_M(NI_element *nel, float *MinFA, float *MaxAngDeg, 
                         float *MinL, int *SeedPerV)
{
   char *atr=NULL;
   
   ENTRY("NI_getTractAlgOpts");
   if (!nel) RETURN(1);
   
   if (MinFA && (atr=NI_get_attribute(nel,"Thresh_FA"))) {
      *MinFA = (float)strtod(atr,NULL); 
   }
   if (MaxAngDeg && (atr=NI_get_attribute(nel,"Thresh_ANG"))) {
      *MaxAngDeg = (float)strtod(atr,NULL);
   }
   if (MinL && (atr=NI_get_attribute(nel,"Thresh_Len"))) {
      *MinL = (float)strtod(atr,NULL);
   }
   if (SeedPerV && (atr=NI_get_attribute(nel,"Nseed_X"))) {
      SeedPerV[0] = (int)strtod(atr,NULL);
   }
   if (SeedPerV && (atr=NI_get_attribute(nel,"Nseed_Y"))) {
      SeedPerV[1] = (int)strtod(atr,NULL);
   }
   if (SeedPerV && (atr=NI_get_attribute(nel,"Nseed_Z"))) {
      SeedPerV[2] = (int)strtod(atr,NULL);
   }
   
   RETURN(0);
}

NI_element * NI_setTractAlgOpts_M(NI_element *nel, float *MinFA, 
										  float *MaxAngDeg, float *MinL, 
                                  int *SeedPerV)
{   
   ENTRY("NI_setTractAlgOpts");
   
   if (!nel) nel = NI_new_data_element ("TRACK_opts",0);
   
   if (MinFA ) {
      NI_SETA_FLOAT(nel,"Thresh_FA",*MinFA);
   }
   if (MaxAngDeg) {
      NI_SETA_FLOAT(nel,"Thresh_ANG",*MaxAngDeg);
   }
   if (MinL) {
      NI_SETA_FLOAT(nel,"Thresh_Len",*MinL);
   }
   if (SeedPerV) {
      NI_SETA_INT(nel,"Nseed_X",SeedPerV[0]);
      NI_SETA_INT(nel,"Nseed_Y",SeedPerV[1]);
      NI_SETA_INT(nel,"Nseed_Z",SeedPerV[2]);
   }
   
   RETURN(nel);
}
      

NI_element * ReadTractAlgOpts_M(char *fname) 
{
   NI_stream ns=NULL;
   NI_element *nel=NULL;
   float MinFA, MaxAngDeg, MinL;
   int SeedPerV[3];
   char *strm=NULL;
   FILE *fin4=NULL;
   
   ENTRY("ReadTractAlgOpts");  
       
   if (!fname || !THD_is_file(fname)) RETURN(NULL);
   
   if (STRING_HAS_SUFFIX(fname,".niml.opts")) {
      strm = (char *)calloc(strlen(fname)+20, sizeof(char));
      sprintf(strm,"file:%s",fname);
      if (!(ns = NI_stream_open( strm , "r" ))) {
         ERROR_message("Failed to open %s\n", strm);
         free(strm); RETURN(NULL);
      }
      if (!(nel = NI_read_element( ns , 2 ))) {
         ERROR_message("Failed to read element from \n", strm);
         free(strm); RETURN(NULL);
      }
      NI_stream_close(ns); free(strm); strm = NULL;
   } else {
      // Opening/Reading in FACT params
      if( (fin4 = fopen(fname, "r")) == NULL) {
			fprintf(stderr, "Error opening file %s.",fname);
			RETURN(NULL);
      }

      if( !(fscanf(fin4, "%f %f %f %d %d %d",
				 &MinFA,&MaxAngDeg,&MinL,&SeedPerV[0],&SeedPerV[1],
                   &SeedPerV[2]))){
         fprintf(stderr, "Error reading parameter files.");
			RETURN(NULL);
      }

      fclose(fin4);
      if (!(nel = 
            NI_setTractAlgOpts_M(NULL, &MinFA, &MaxAngDeg, &MinL, 
                               SeedPerV))){
         ERROR_message("Failed to get options");
         RETURN(NULL);
      }
   }
   
   RETURN(nel);
}      

// THIS one doesn't need to change with new format for MULTI/HARDI update
int WriteTractAlgOpts(char *fname, NI_element *nel) 
{
   char *strm=NULL;
   NI_stream ns=NULL;
   
   ENTRY("WriteTractAlgOpts");
   
   if (!nel) {
      fprintf(stderr, "NULL nel\n");
      RETURN(1);
   }
   if (fname) {
      strm = (char *)calloc(strlen(fname)+20, sizeof(char));
      if (STRING_HAS_SUFFIX(fname,".niml.opts")) {
         sprintf(strm,"file:%s",fname);
      } else {
         sprintf(strm,"file:%s.niml.opts",fname);
      }
   } else {
      strm = (char *)calloc(20, sizeof(char));
      sprintf(strm,"fd:1");
   }
   if (!(ns = NI_stream_open( strm , "w" ))) {
      ERROR_message("Failed to open %s\n", strm);
      free(strm); RETURN(1);
   }
   NI_write_element(ns,nel,NI_TEXT_MODE);
   NI_stream_close(ns); free(strm); strm = NULL;
   RETURN(0);
} 

int NI_getProbTractAlgOpts_M(NI_element *nel, float *MinFA, float *MaxAngDeg, 
									float *MinL, float *NmNsFr, int *Nseed, 
									int *Nmonte)
{
   char *atr=NULL;
   
   ENTRY("NI_getProbTractAlgOpts");
   if (!nel) RETURN(1);
   
   if (MinFA && ( (atr=NI_get_attribute(nel,"Thresh_FA")) ||
                  (atr=NI_get_attribute(nel,"MinFA"))) ) {
      *MinFA = (float)strtod(atr,NULL);
   }
   if (MaxAngDeg && ( (atr=NI_get_attribute(nel,"Thresh_ANG")) ||
                      (atr=NI_get_attribute(nel,"MaxAng"))) ) {
      *MaxAngDeg = (float)strtod(atr,NULL);
   }
   if (MinL && ( (atr=NI_get_attribute(nel,"Thresh_Len")) ||
                 (atr=NI_get_attribute(nel,"MinL"))) ) {
      *MinL = (float)strtod(atr,NULL);
   }

   if (NmNsFr && ( (atr=NI_get_attribute(nel,"Thresh_Frac")) ||
                   (atr=NI_get_attribute(nel,"MinHitFr"))) ) {
      *NmNsFr = (float)strtod(atr,NULL);
   }
   if (Nseed && ( (atr=NI_get_attribute(nel,"Nseed_Vox")) ||
                  (atr=NI_get_attribute(nel,"Nseed"))) ) {
	   *Nseed = (int)strtod(atr,NULL);
   }
   if (Nmonte && (atr=NI_get_attribute(nel,"Nmonte"))) {
      *Nmonte = (int)strtod(atr,NULL);
   }
   RETURN(0);
}

NI_element * NI_setProbTractAlgOpts_M(NI_element *nel, float *MinFA, 
												float *MaxAngDeg, float *MinL,
												float *NmNsFr, int *Nseed, 
												int *Nmonte)
{   
   ENTRY("NI_setProbTractAlgOpts_M");
   
   if (!nel) nel = NI_new_data_element ("PROBTRACK_opts",0);
   
   if (MinFA ) {
      NI_SETA_FLOAT(nel,"Thresh_FA",*MinFA);
   }
   if (MaxAngDeg) {
      NI_SETA_FLOAT(nel,"Thresh_ANG",*MaxAngDeg);
   }
   if (MinL) {
      NI_SETA_FLOAT(nel,"Thresh_Len",*MinL);
   }
   if (NmNsFr) {
	   NI_SETA_FLOAT(nel,"Thresh_Frac",*NmNsFr);
   }
	if (Nseed) {
	   NI_SETA_INT(nel,"Nseed_Vox",*Nseed);
   }
	if (Nmonte) {
      NI_SETA_INT(nel,"Nmonte",*Nmonte);
	}
   
   RETURN(nel);
}

NI_element * ReadProbTractAlgOpts_M(char *fname) 
{
   NI_stream ns=NULL;
   NI_element *nel=NULL;
   float MinFA, MaxAngDeg, MinL,NmNsFr;
   int Nseed, Nmonte;
   char *strm=NULL;
   FILE *fin4=NULL;
   
   ENTRY("ReadProbTractAlgOpts");  
       
   if (!fname || !THD_is_file(fname)) RETURN(NULL);
   
   if (STRING_HAS_SUFFIX(fname,".niml.opts")) {
      strm = (char *)calloc(strlen(fname)+20, sizeof(char));
      sprintf(strm,"file:%s",fname);
      if (!(ns = NI_stream_open( strm , "r" ))) {
         ERROR_message("Failed to open %s\n", strm);
         free(strm); RETURN(NULL);
      }
      if (!(nel = NI_read_element( ns , 2 ))) {
         ERROR_message("Failed to read element from \n", strm);
         free(strm); RETURN(NULL);
      }
      NI_stream_close(ns); free(strm); strm = NULL;
   } else {
      // Opening/Reading in FACT params
      if( (fin4 = fopen(fname, "r")) == NULL) {
			fprintf(stderr, "Error opening file %s.",fname);
			RETURN(NULL);
      }

      if( !(fscanf(fin4, "%f %f %f %f %d %d",
                   &MinFA,&MaxAngDeg,&MinL,&NmNsFr,&Nseed,&Nmonte))){
         fprintf(stderr, "Error reading parameter files.");
			RETURN(NULL);
      }
      fclose(fin4);
      //printf("%f %f %f %f %d %d",
      //     MinFA,MaxAngDeg,MinL,NmNsFr,Nseed,Nmonte); 

      if (!(nel = 
            NI_setProbTractAlgOpts_M(NULL, &MinFA, &MaxAngDeg, &MinL, 
											  &NmNsFr,&Nseed,&Nmonte))){
         ERROR_message("Failed to get options");
         RETURN(NULL);
      }
   }
   
   RETURN(nel);
}      

int SimpleWriteDetNetTr_M(int N_HAR, FILE *file, int ***idx, 
                           THD_3dim_dataset **PARS,
                           int PAR_BOT, int PAR_TOP,
                           float **loc, int **locI, int len,
                           int *TV, int *Dim, float *Ledge)
{ // for trackvis
   int m,aa,bb;
  int READS_in;
  float READS_fl;

  ENTRY("SimpleWriteDetNetTr");
  
  // first write len of tr
  READS_in = len;
  fwrite(&READS_in,sizeof(READS_in),1,file);

  // then track, coors and props
  for( m=0 ; m<len ; m++ ) {
    // recenter phys loc for trackvis, if nec...
    for( aa=0 ; aa<3 ; aa++ ) {
      READS_fl = loc[m][aa];
      if(!TV[aa])
        READS_fl = Ledge[aa]*Dim[aa]-READS_fl;	
      fwrite(&READS_fl,sizeof(READS_fl),1,file);
    }
    bb=idx[locI[m][0]][locI[m][1]][locI[m][2]];
    if(N_HAR) { // hardi, single param
       READS_fl = THD_get_voxel(PARS[PAR_BOT], bb, 0); 
       fwrite(&READS_fl,sizeof(READS_fl),1,file);
    }
    else 
       for( aa=1 ; aa<4 ; aa++ ) {
          READS_fl = THD_get_voxel(PARS[aa], bb, 0); 
          fwrite(&READS_fl,sizeof(READS_fl),1,file);
       }
  }
  
  RETURN(1);
}

int Free_Insta_Tract_Setup(INSTA_TRACT_SETUP *ITS)
{
   ENTRY("Free_Insta_Tract_Setup");

   if (!ITS) RETURN(0);
   
   if (ITS->grid) DSET_delete(ITS->grid);
   ITS->grid = NULL;
   
   /* Do not delte ITS , leave it to calling function */
  
   RETURN(1);
}

/* Create brandnew, or wipe clean existing ITS */
INSTA_TRACT_SETUP *New_Insta_Tract_Setup(INSTA_TRACT_SETUP *ITS)
{
   ENTRY("New_Insta_Tract_Setup");

   if (!ITS) ITS = (INSTA_TRACT_SETUP *)calloc(1,sizeof(INSTA_TRACT_SETUP));
   else Free_Insta_Tract_Setup(ITS);
   
   /* Put any initialization here ... */
   
   RETURN(ITS);
}


// *****************-> NIMLly reading in DTI input  <-*********************

NI_element * ReadDTI_inputs(char *fname) 
{
   NI_stream ns=NULL;
   NI_element *nel=NULL;
   char *strm=NULL;
   FILE *fin4=NULL;
   
   ENTRY("ReadDTI_inputs");  
       
   if (!fname || !THD_is_file(fname)) RETURN(NULL);
   
   if (STRING_HAS_SUFFIX(fname,".niml.opts")) {
      strm = (char *)calloc(strlen(fname)+20, sizeof(char));
      sprintf(strm,"file:%s",fname);
      if (!(ns = NI_stream_open( strm , "r" ))) {
         ERROR_message("Failed to open %s\n", strm);
         free(strm); RETURN(NULL);
      }
      if (!(nel = NI_read_element( ns , 2 ))) {
         ERROR_message("Failed to read element from \n", strm);
         free(strm); RETURN(NULL);
      }
      NI_stream_close(ns); free(strm); strm = NULL;
   } else {
      ERROR_message("Failed to get DTI inputs from %s",fname);
      RETURN(NULL);
   }
   
   RETURN(nel);
}      


int NI_getDTI_inputs( NI_element *nel, 
                      char **NameVECT,
                      char *NameXF, 
                      char **NameSCAL, 
                      char **NamePLUS,
                      int *extrafile, int *pars_top)
{
   char *atr="NONAME";
   char tmp[THD_MAX_PREFIX];
   int i;
   int ct_scal = 1;  // 1 -> start with space for 'extrafile' 

   ENTRY("NI_getDTI_inputs");
   if (!nel) RETURN(1);

   atr = (char *)calloc(100, sizeof(char)); 
   if( (atr == NULL) ) {
      fprintf(stderr, "\n\n MemAlloc failure.\n\n");
      exit(126);
   }

   // for vectors
   for( i=0 ; i<N_DTI_VECT ; i++ ) {
      sprintf(tmp, "dti_%s", DTI_VECT_LABS[i]);
      if (NameVECT[i] && (atr=NI_get_attribute(nel,tmp))) {
         snprintf(NameVECT[i], N_CHAR_PATH, "%s", atr);
      }
   }
         INFO_message(" CCC1  ");

   // for scalars
   for( i=0 ; i<N_DTI_SCAL ; i++ ) {
      sprintf(tmp, "dti_%s", DTI_SCAL_LABS[i]);
      if (NameSCAL[i] && (atr=NI_get_attribute(nel,tmp))) {
         snprintf(NameSCAL[i], N_CHAR_PATH, "%s", atr);
         ct_scal++;
      }
   }

   // for extra scalar
   sprintf(tmp, "dti_%s", DTI_XTRA_LABS[0]);
   if (NameXF && (atr=NI_get_attribute(nel,tmp))) {
      snprintf(NameXF, N_CHAR_PATH, "%s", atr); 
      *extrafile = 1;
   }
   else
      NameXF = NULL;
   
   
   // allow up to four extra files
   for( i=0 ; i<N_DTI_PLUS ; i++ ) {
      sprintf(tmp, "dti_%s", DTI_PLUS_LABS[i]);
      if (NamePLUS[i] && (atr=NI_get_attribute(nel,tmp))) {
         snprintf(NamePLUS[i], N_CHAR_PATH, "%s", atr);
         ct_scal++;
      }
      else 
         snprintf(NamePLUS[i], N_CHAR_PATH, "%s", "\0");
   }
   
   *pars_top = ct_scal; // 2 + 3 + ct_ex; // RD and extra; FA,MD;extras
   INFO_message(" ct_scal: %d atr:%s ", ct_scal, atr);
   
   RETURN(0);
}


char *SUMA_Taylor_Bundle_Info(TAYLOR_BUNDLE *tb, int show_maxu) 
{
   static char FuncName[]={"SUMA_Taylor_Bundle_Info"};
   int show_max;
   int ii=0;
   char stmp[64];
   char *s=NULL;
   SUMA_STRING *SS=NULL;
   
   SUMA_ENTRY;

   SS = SUMA_StringAppend(NULL, NULL);

   if (!tb) {
      SUMA_StringAppend(SS,"NULL bundle pointer"); 
   } else {
      SUMA_StringAppend_va(SS, "Bundle has %d tracts\n", tb->N_tracts);
      if ((show_maxu < 0) || (tb->N_tracts < show_maxu)) show_max = tb->N_tracts;
      else if (show_maxu == 0) show_max = (tb->N_tracts < 5) ? tb->N_tracts : 5;
      else show_max = show_maxu < tb->N_tracts ? show_maxu : tb->N_tracts;

      s = NULL;
      for (ii=0; ii<show_max; ++ii) {
         snprintf(stmp, 62,"      Bun.Trc %d ++> ", ii);
         s = SUMA_append_replace_string(s,
               SUMA_Taylor_Tract_Info(tb->tracts+ii, show_maxu),stmp,2);
      }
      SUMA_StringAppend_va(SS,s); SUMA_ifree(s);
      if (show_max < tb->N_tracts) {
         int pl = (tb->N_tracts-show_max > 1);
         SUMA_StringAppend_va(SS,
               "   ... %d tract%sremain%s in bundle.\n", 
               tb->N_tracts-show_max, pl ? "s ":" ", pl ? "":"s");
      }
   }
   
   SUMA_SS2S(SS, s);
   SUMA_RETURN(s);
}



char *SUMA_Taylor_Tract_Info(TAYLOR_TRACT *tt, int show_maxu) 
{
   static char FuncName[]={"SUMA_Taylor_Tract_Info"};
   int show_max;
   int ii=0;
   char *s=NULL;
   SUMA_STRING *SS=NULL;
   
   SUMA_ENTRY;

   SS = SUMA_StringAppend(NULL, NULL);

   if (!tt) {
      SUMA_StringAppend(SS,"NULL tract pointer"); 
   } else {
      SUMA_StringAppend_va(SS, "  track id %d, Npts=%d\n", 
                           tt->id, TRACT_NPTS(tt));
      if (show_maxu < 0) show_max = TRACT_NPTS(tt);
      else if (show_maxu == 0) 
                  show_max = (TRACT_NPTS(tt) < 5) ? TRACT_NPTS(tt) : 5;
      else show_max = show_maxu < TRACT_NPTS(tt) ? show_maxu : TRACT_NPTS(tt);
      for (ii=0; ii<show_max; ++ii) {
         SUMA_StringAppend_va(SS, "      %d %f %f %f\n", 
                 ii, tt->pts[3*ii], tt->pts[3*ii+1],tt->pts[3*ii+2]);
      }
      if (show_max < TRACT_NPTS(tt)) {
         int pl = (TRACT_NPTS(tt)-show_max > 1);
         SUMA_StringAppend_va(SS, 
                     "      ... %d point%sremain%s in tract.\n",
            TRACT_NPTS(tt)-show_max, pl ? "s ":" ", pl ? "":"s");
      }
   }
   
   SUMA_SS2S(SS, s);
   SUMA_RETURN(s);
}


/* Both of SUMA_Network_N_points and SUMA_Network_N_tracts 
   are copies of the one in ptaylor/TrackIO.c
   The other ones should be used as soon as 
   we resolve afni's dependency on ptaylor/ */
int SUMA_Network_N_points(TAYLOR_NETWORK *net, byte recalc)
{
   int nb, ib=0, nn=-1, it;
   
   if (!net) return(-1);
   if (!recalc && net->N_points_private > 0) return(net->N_points_private);
   nn = 0;
   for (ib=0; ib<net->N_tbv; ++ib) {
      if (net->tbv[ib]) {
         nb = 0;
         for (it=0; it<net->tbv[ib]->N_tracts; ++it) {
            nb += net->tbv[ib]->tracts[it].N_pts3;
         }
         net->tbv[ib]->N_points_private = nb/3;
         nn += nb;
      } 
   }
   nn /= 3;
   net->N_points_private = nn;
   return(nn);
}

int SUMA_Network_N_tracts(TAYLOR_NETWORK *net, byte recalc)
{
   int nb, ib=0, nn=-1, it;
   
   if (!net) return(-1);
   if (!recalc && net->N_tracts_private > 0) return(net->N_tracts_private);
   nn = 0;
   for (ib=0; ib<net->N_tbv; ++ib) {
      if (net->tbv[ib]) {
         nn += net->tbv[ib]->N_tracts;
      } 
   }
   net->N_tracts_private = nn;
   return(nn);
}


char * SUMA_Taylor_Network_Info(TAYLOR_NETWORK *net, 
                                int show_maxu, int show_maxub)
{
   static char FuncName[]={"SUMA_Taylor_Network_Info"};
   TAYLOR_BUNDLE *tb=NULL;
   int show_max;
   char stmp[64];
   int ii=0;
   char *s=NULL;
   SUMA_STRING *SS=NULL;
   
   SUMA_ENTRY;
   
   SS = SUMA_StringAppend(NULL, NULL);

   if (!net) {
      SUMA_StringAppend(SS,"NULL network pointer"); 
   } else {
      SUMA_StringAppend_va(SS,"  Network has %d bundles, %d tracts, %d points\n",
            net->N_tbv, SUMA_Network_N_tracts(net,1), 
            SUMA_Network_N_points(net, 1));
      if (show_maxu < 0) show_max = net->N_tbv;
      else if (show_maxu == 0) show_max = (net->N_tbv < 5) ? net->N_tbv : 5;  
      else show_max = show_maxu < net->N_tbv ? show_maxu : net->N_tbv;

      s = NULL;
      for (ii=0; ii<show_max; ++ii) {
         snprintf(stmp, 62, "   Net.Bun. %d --> ", ii);
         s = SUMA_append_replace_string(s,
               SUMA_Taylor_Bundle_Info(net->tbv[ii], show_maxub),stmp,2);
      }
      SUMA_StringAppend_va(SS, s); SUMA_ifree(s);
      if (show_max < net->N_tbv) {
         int pl = (net->N_tbv-show_max > 1);
         SUMA_StringAppend_va(SS, 
            "... %d bundle%sremain%s in network.\n", 
            net->N_tbv-show_max, pl ? "s ":" ", pl ? "":"s");
      }
      
   }
     
   SUMA_SS2S(SS, s);
   SUMA_RETURN(s);
}
//OLD
/*int NI_getDTI_inputs( NI_element *nel, 
                      char **NameVEC,
                      char *NameXF, 
                      char **NameSCAL, 
                      char **NameP,
                      int *extrafile, int *pars_top)
{
   char *atr=NULL;
   int ct_ex = 0;

   ENTRY("NI_getDTI_inputs");
   if (!nel) RETURN(1);
   
   if (NameVEC[0] && (atr=NI_get_attribute(nel,"dti_V1"))) {
      snprintf(NameVEC[0],100,"%s", atr);
   }
   if (NameVEC[1] && (atr=NI_get_attribute(nel,"dti_V2"))) {
      snprintf(NameVEC[1],100,"%s", atr);
   }
   if (NameVEC[2] && (atr=NI_get_attribute(nel,"dti_V3"))) {
      snprintf(NameVEC[2],100,"%s", atr);
   }
   if (NameXF && (atr=NI_get_attribute(nel,"dti_XF"))) {
      snprintf(NameXF,100,"%s", atr); *extrafile = 1;
   }
   else
      NameXF = NULL;
   if (NameSCAL[0] && (atr=NI_get_attribute(nel,"dti_FA"))) {
      snprintf(NameSCAL[0],100,"%s", atr);
   }
   if (NameSCAL[1] && (atr=NI_get_attribute(nel,"dti_MD"))) {
      snprintf(NameSCAL[1],100,"%s", atr);
   }
   if (NameSCAL[2] && (atr=NI_get_attribute(nel,"dti_L1"))) {
      snprintf(NameSCAL[2],100,"%s", atr);
   }
   // allow up to four extra files
   if (NameP[0] && (atr=NI_get_attribute(nel,"dti_P1"))) {
      snprintf(NameP[0],100,"%s", atr);  ct_ex++;
   }
   else snprintf(NameP[0],100,"%s", "\0");
   if (NameP[1] && (atr=NI_get_attribute(nel,"dti_P2"))) {
      snprintf(NameP[1],100,"%s", atr);  ct_ex++;
   }
   else snprintf(NameP[1],100,"%s", "\0");
   if (NameP[2] && (atr=NI_get_attribute(nel,"dti_P3"))) {
      snprintf(NameP[2],100,"%s", atr);  ct_ex++;
   }
   else snprintf(NameP[2],100,"%s", "\0");
   if (NameP[3] && (atr=NI_get_attribute(nel,"dti_P4"))) {
      snprintf(NameP[3],100,"%s", atr);  ct_ex++;
      //printf("\nHERE! %d\n",ct_ex);
   }
   else snprintf(NameP[3],100,"%s", "\0");
   
   *pars_top = 2 + 3 + ct_ex; // RD and extra; FA,MD;extras

   //printf("\nLISTED: PARS_TOP=%d with ct_ex=%d\n", *pars_top, ct_ex);
   //   printf("\nLISTED: NameSCAL[0]=%s\n", NameSCAL[0]);


   RETURN(0);
}

*/
