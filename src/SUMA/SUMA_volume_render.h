#ifndef SUMA_VOLUME_INCLUDED
#define SUMA_VOLUME_INCLUDED


SUMA_Boolean SUMA_Draw3DTextureNIDOnel(  NI_element *nel, 
                                    SUMA_SurfaceObject *SO, 
                                    SUMA_DO_CoordUnits default_coord_type,
                                    float *default_txcol, 
                                    void *default_font, int default_node,
                                    SUMA_SurfaceViewer *sv) ;

SUMA_Boolean SUMA_Load3DTextureNIDOnel (NI_element *nel,
                                 SUMA_DO_CoordUnits coordtype);
SUMA_EnablingRecord SUMA_RecordEnablingState(void);
void SUMA_RestoreEnablingState(SUMA_EnablingRecord SER);
void SUMA_ShowEnablingState(SUMA_EnablingRecord SER, FILE *out, char *preamble);
char *SUMA_EnablingState_Info(SUMA_EnablingRecord SER);
void SUMA_dset_extreme_corners( THD_3dim_dataset *dset, 
                                float * mincorner, float *maxcorner);
void SUMA_dset_tex_slice_corners( int slc, THD_3dim_dataset *dset, 
                              GLfloat *tcorners, GLfloat *corners, int dim);
SUMA_Boolean SUMA_SetTextureClipPlaneSurface(
                        SUMA_VolumeObject *VO, int iplane );
SUMA_SurfaceObject **SUMA_TextureClipPlaneSurfaces(int *N_SOlist);
SUMA_VolumeObject *SUMA_VolumeObjectOfClipPlaneSurface(SUMA_SurfaceObject *SO);
GLubyte * SUMA_dset_to_tex3d(THD_3dim_dataset **dset, byte col);
int SUMA_MoveCutplane (SUMA_VolumeObject *VO, int iplane, float d);
SUMA_Boolean SUMA_DrawVolumeDO(SUMA_VolumeObject *VO, SUMA_SurfaceViewer *sv);
NI_element *SUMA_3DTextureNIDOnelofVO(SUMA_VolumeObject *VO) ;
SUMA_VolumeObject *SUMA_VOof3DTextureNIDOnel(NI_element *nel);
SUMA_Boolean SUMA_Colorize_dset(THD_3dim_dataset *dset, 
                                 byte *tex3ddata, byte colopt);
int iPlane2Dim(int iplane);
#endif
