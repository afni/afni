#ifndef SUMA_VOLUME_INCLUDED
#define SUMA_VOLUME_INCLUDED


SUMA_Boolean SUMA_Draw3DTextureNIDOnel(  NI_element *nel, 
                                    SUMA_SurfaceObject *SO, 
                                    SUMA_DO_CoordUnits default_coord_type,
                                    float *default_txcol, 
                                    void *default_font, int default_node,
                                    SUMA_SurfaceViewer *sv) ;
SUMA_Boolean SUMA_VE_LoadTexture(SUMA_VolumeElement **VE, int n);
SUMA_Boolean SUMA_VO_InitCutPlanes(SUMA_VolumeObject *VO);
SUMA_Boolean SUMA_CreateGL3DTexture(SUMA_VolumeObject *VO);
SUMA_Boolean SUMA_Load3DTextureNIDOnel (NI_element *nel,
                                 SUMA_DO_CoordUnits coordtype);
void SUMA_RecordEnablingState(SUMA_EnablingRecord *SER);
void SUMA_RestoreEnablingState(SUMA_EnablingRecord *SER);
void SUMA_ShowEnablingState(SUMA_EnablingRecord *SER, FILE *out, char *preamble);
char *SUMA_EnablingState_Info(SUMA_EnablingRecord *SER);
SUMA_Boolean SUMA_dset_box_corners( SUMA_DSET *dset, 
                                    float * corners, int voxcen);
void SUMA_dset_extreme_corners( SUMA_DSET *dset, 
                                float * mincorner, float *maxcorner,
                                int voxcen);
int SUMA_dset_gui_slice_from_tex_slice_d(SUMA_VolumeElement **VE, int ive,
                     double *PlEq, int voxcen,
                     char *variant,int *slider);
int SUMA_dset_gui_slice_from_tex_slice(SUMA_VolumeElement **VE, int ive,
                     float *PlEq, int voxcen,
                     char *variant,int *slider);
int SUMA_dset_tex_slice_corners_gui(SUMA_VolumeElement **VE, int ive, 
                                             char *variant,int slider, 
                          GLfloat *tcorners, GLfloat *corners, GLfloat *slc_cen, 
                          float *PlEq, int voxcen );
SUMA_Boolean SUMA_GET_VR_Slice_Pack(SUMA_VolumeObject *VO, 
                                    SUMA_SurfaceViewer *sv);
SUMA_Boolean SUMA_Get_Slice_Pack(SUMA_VolumeObject *VO, 
                                 char *variant, SUMA_SurfaceViewer *sv);
void SUMA_dset_tex_slice_corners( int slc, SUMA_DSET *dset, 
                              GLfloat *tcorners, GLfloat *corners, 
                              GLfloat *slccen, int dim,int voxcen);
void SUMA_dset_tex_slice_corners_card( int slc, THD_3dim_dataset *dset, 
                              GLfloat *tcorners, GLfloat *corners, int dim,
                              int voxcen);
int SUMA_VO_SelectedSlice(SUMA_VolumeObject *vo, char *variant, float *scorners);
SUMA_Boolean SUMA_SetTextureClipPlaneSurface(
                        SUMA_VolumeObject *VO, int iplane );
SUMA_SurfaceObject **SUMA_TextureClipPlaneSurfaces(int *N_SOlist);
SUMA_VolumeObject *SUMA_VolumeObjectOfClipPlaneSurface(SUMA_SurfaceObject *SO);
SUMA_DSET *SUMA_adset_to_VE(SUMA_VolumeObject *VO, THD_3dim_dataset **dsetp);
GLubyte * SUMA_VE_to_tex3d(SUMA_VolumeElement **VE, int iVE, byte col);
int SUMA_MoveCutplane (SUMA_VolumeObject *VO, int iplane, float d);
SUMA_Boolean SUMA_DrawVolumeDO(SUMA_VolumeObject *VO, SUMA_SurfaceViewer *sv);
SUMA_Boolean SUMA_DrawVolumeDO_slices(SUMA_VolumeObject *VO, 
                                    SUMA_SurfaceViewer *sv);
SUMA_Boolean SUMA_DrawVolumeDO_3D(SUMA_VolumeObject *VO, 
                                    SUMA_SurfaceViewer *sv);
SUMA_Boolean SUMA_DrawVolumeDO_exp(SUMA_VolumeObject *VO, 
                                   SUMA_SurfaceViewer *sv);
NI_element *SUMA_3DTextureNIDOnelofVO(SUMA_VolumeObject *VO) ;
SUMA_VolumeObject *SUMA_VOof3DTextureNIDOnel(NI_element *nel);
SUMA_Boolean SUMA_Colorize_dset(SUMA_DSET *dset, 
                                 byte *tex3ddata, byte colopt);
int iPlane2Dim(int iplane);
#endif
