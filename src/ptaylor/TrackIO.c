#include <afni.h> 
#include <suma_afni_surface.h> 
#include <suma_algorithms.h> 
#include <suma_datasets.h> 
#include <TrackIO.h>

static int NI_tract_type = -1;
int get_NI_tract_type(void) {
   if (NI_tract_type == -1) {
      if ((NI_tract_type = NI_rowtype_define("TAYLOR_TRACT_DATUM", 
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
   int nn;
   TAYLOR_TRACT *tt=NULL;
   
   ENTRY("AppCreateBundle");
   
   if (!tbu) {
      tb = (TAYLOR_BUNDLE *)calloc(1,sizeof(TAYLOR_BUNDLE));
      tb->N_allocated = 0;
      tb->N_tracts = 0;
   } else {
      tb = tbu;
   }
   while (N_tractsbuf > tb->N_allocated - tb->N_tracts) {
      tb->N_allocated += 1000;
      tb->tracts = (TAYLOR_TRACT*)realloc(tb->tracts,
                                          tb->N_allocated*sizeof(TAYLOR_TRACT));
   }
   
   if (tracts_buff && N_tractsbuf > 0) {
      for (nn=0; nn<N_tractsbuf; ++nn) {
         tt = tb->tracts+nn+tb->N_tracts; 
         tt->id = tracts_buff[nn].id;
         tt->N_pts3 = tracts_buff[nn].N_pts3;
         tt->pts = (float *)calloc(tt->N_pts3, sizeof(float));
         if (tract_verb > 1 && nn<3) {
            fprintf(stderr,"AppCreateBundle %d , id %d, N_pts %d, pts %p\n",
						  nn, tt->id, TRACT_NPTS(tt), tracts_buff[nn].pts);
         }
         memcpy(tt->pts, tracts_buff[nn].pts, tt->N_pts3*sizeof(float));
      }
      tb->N_tracts += N_tractsbuf;
   } 
   
   RETURN(tb);
}

/*!
  grid (THD_3dim_dataset *) Without the grid structure, coordinates
  will not be in RAI, but in UHU, unholy units.
*/
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
}

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
   fprintf(out,"  Bundle has %d tracts\n", tb->N_tracts);
   if (show_maxu < 0) show_max = tb->N_tracts;
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
   if (!tb) {
      fprintf(out,"NULL tb"); 
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
   
   ENTRY("Bundle_2_NIgr");
   
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

TAYLOR_NETWORK *NIgr_2_Network(NI_group *ngr) 
{
   TAYLOR_NETWORK *net=NULL;
   TAYLOR_BUNDLE *tbb=NULL; 
   TAYLOR_TRACT *tt=NULL;
   NI_element *nel=NULL;
   int ip=0, N_tracts=0, ei=0;
   char *bad=NULL, *sbuf=NULL;
   
   ENTRY("NIgr_2_Network");

   if (!ngr) RETURN(net);
   if (!strcmp(ngr->name,"bundle") ||/* old style */
       !strcmp(ngr->name,"network") ) { 
      net = (TAYLOR_NETWORK *)calloc(1,sizeof(TAYLOR_NETWORK));
      tbb = (TAYLOR_BUNDLE *)calloc(1,sizeof(TAYLOR_BUNDLE));
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
						tbb = AppCreateBundle(tbb, N_tracts, tt); 
						tt = Free_Tracts(tt, N_tracts);
                  NI_GET_INT(nel,"Bundle_Tag",ei);
                  if (!NI_GOT) ei = -1;
                  net = AppAddBundleToNetwork(net, &tbb, ei, -1, NULL);
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
                                      THD_3dim_dataset *grid)
{
   TAYLOR_NETWORK *net=NULL;
   
   ENTRY("AppAddBundleToNetwork");
   
   if (!tb) RETURN(net);
   
   if (!network) {
      net = (TAYLOR_NETWORK *)calloc(1,sizeof(TAYLOR_NETWORK));
      net->N_allocated = -1;
      if (grid) {
         snprintf(net->atlas_space,64*sizeof(char),"%s", grid->atlas_space);
      } else {
         snprintf(net->atlas_space,64*sizeof(char),"UNKNOWN");
      }      
   } else { 
      net = network;
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


int NI_getTractAlgOpts(NI_element *nel, float *MinFA, float *MaxAngDeg, 
                       float *MinL, int *SeedPerV, int *M, int *bval)
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
   if (M && (atr=NI_get_attribute(nel,"Ngrads"))) {
      *M = (int)strtod(atr,NULL);
   }
   if (bval && (atr=NI_get_attribute(nel,"Bval"))) {
      *bval = (int)strtod(atr,NULL);
   }
   RETURN(0);
}

NI_element * NI_setTractAlgOpts(NI_element *nel, float *MinFA, 
										  float *MaxAngDeg, float *MinL, 
										  int *SeedPerV, int *M, int *bval)
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
   if (M) {
      NI_SETA_INT(nel,"Ngrads",*M);
   }
   if (bval) {
      NI_SETA_INT(nel,"Bval",*bval);
   }
   
   RETURN(nel);
}

NI_element * ReadTractAlgOpts(char *fname) 
{
   NI_stream ns=NULL;
   NI_element *nel=NULL;
   float MinFA, MaxAngDeg, MinL;
   int SeedPerV[3], M, bval;
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
      fscanf(fin4, "%f %f %f %d %d %d %d %d",
				 &MinFA,&MaxAngDeg,&MinL,&SeedPerV[0],&SeedPerV[1],
				 &SeedPerV[2],&M,&bval);
      fclose(fin4);
      if (!(nel = 
            NI_setTractAlgOpts(NULL, &MinFA, &MaxAngDeg, &MinL, 
                               SeedPerV, &M, &bval))){
         ERROR_message("Failed to get options");
         RETURN(NULL);
      }
   }
   
   RETURN(nel);
}      

int WriteTractAlgOpts(char *fname, NI_element *nel) 
{
   char *strm=NULL;
   NI_stream ns=NULL;
   
   ENTRY("WriteTractAlgOpts");
   
   if (!nel || !fname) RETURN(1);
   
   strm = (char *)calloc(strlen(fname)+20, sizeof(char));
   if (STRING_HAS_SUFFIX(fname,".niml.opts")) {
      sprintf(strm,"file:%s",fname);
   } else {
      sprintf(strm,"file:%s.niml.opts",fname);
   }
   if (!(ns = NI_stream_open( strm , "w" ))) {
      ERROR_message("Failed to open %s\n", strm);
      free(strm); RETURN(1);
   }
   NI_write_element(ns,nel,NI_TEXT_MODE);
   NI_stream_close(ns); free(strm); strm = NULL;
   RETURN(0);
} 

// **********************************

int NI_getProbTractAlgOpts(NI_element *nel, float *MinFA, float *MaxAngDeg, 
									float *MinL, float *NmNsFr, int *Nseed, 
									int *Nmonte, int *M, int *bval)
{
   char *atr=NULL;
   
   ENTRY("NI_getProbTractAlgOpts");
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
   if (NmNsFr && (atr=NI_get_attribute(nel,"Thresh_Frac"))) {
      *NmNsFr = (float)strtod(atr,NULL);
   }
   if (Nseed && (atr=NI_get_attribute(nel,"Nseed_Vox"))) {
	   *Nseed = (int)strtod(atr,NULL);
   }
   if (Nmonte && (atr=NI_get_attribute(nel,"Nmonte"))) {
      *Nmonte = (int)strtod(atr,NULL);
   }
   if (M && (atr=NI_get_attribute(nel,"Ngrads"))) {
      *M = (int)strtod(atr,NULL);
   }
   if (bval && (atr=NI_get_attribute(nel,"Bval"))) {
      *bval = (int)strtod(atr,NULL);
   }
   RETURN(0);
}

NI_element * NI_setProbTractAlgOpts(NI_element *nel, float *MinFA, 
												float *MaxAngDeg, float *MinL,
												float *NmNsFr, int *Nseed, 
												int *Nmonte, int *M, int *bval)
{   
   ENTRY("NI_setProbTractAlgOpts");
   
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
   if (M) {
      NI_SETA_INT(nel,"Ngrads",*M);
   }
   if (bval) {
      NI_SETA_INT(nel,"Bval",*bval);
   }
   
   RETURN(nel);
}

NI_element * ReadProbTractAlgOpts(char *fname) 
{
   NI_stream ns=NULL;
   NI_element *nel=NULL;
   float MinFA, MaxAngDeg, MinL,NmNsFr;
   int Nseed, Nmonte, M, bval;
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
      fscanf(fin4, "%f %f %f %f %d %d %d %d",
             &MinFA,&MaxAngDeg,&MinL,&NmNsFr,&Nseed,&Nmonte,&M,&bval);
      fclose(fin4);
      if (!(nel = 
            NI_setProbTractAlgOpts(NULL, &MinFA, &MaxAngDeg, &MinL, 
											  &NmNsFr,&Nseed,&Nmonte,&M,&bval))){
         ERROR_message("Failed to get options");
         RETURN(NULL);
      }
   }
   
   RETURN(nel);
}      

int SimpleWriteDetNetTr(FILE *file, int ***idx, THD_3dim_dataset *FA,
                        THD_3dim_dataset *MD, THD_3dim_dataset *L1,
                        float **loc, int **locI, int len,
                        int *TV, int *Dim, float *Ledge)
{ // for trackvis
  int m,aa,bb;
  int READS_in;
  float READS_fl;

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
    READS_fl = 1; //THD_get_voxel(FA, bb, 0); 
    fwrite(&READS_fl,sizeof(READS_fl),1,file);
    READS_fl = 1; //THD_get_voxel(MD, bb, 0); 
    fwrite(&READS_fl,sizeof(READS_fl),1,file);
    READS_fl = 1; //THD_get_voxel(L1, bb, 0); 
    fwrite(&READS_fl,sizeof(READS_fl),1,file);
  }

  RETURN(1);
}
