#ifndef SUMA_VOLUME_INCLUDED
#define SUMA_VOLUME_INCLUDED


SUMA_Boolean SUMA_Draw3DTextureNIDOnel(  NI_element *nel, 
                                    SUMA_SurfaceObject *SO, 
                                    SUMA_DO_CoordUnits default_coord_type,
                                    float *default_txcol, 
                                    void *default_font,
                                    SUMA_SurfaceViewer *sv) ;

SUMA_Boolean SUMA_Load3DTextureNIDOnel (NI_element *nel);
SUMA_EnablingRecord SUMA_RecordEnablingState(void);
void SUMA_RestoreEnablingState(SUMA_EnablingRecord SER);
void SUMA_ShowEnablingState(SUMA_EnablingRecord SER, FILE *out, char *preamble);
char *SUMA_EnablingState_Info(SUMA_EnablingRecord SER);
void SUMA_dset_extreme_corners( THD_3dim_dataset *dset, 
                                float * mincorner, float *maxcorner);
void SUMA_dset_tex_slice_corners( int slc, THD_3dim_dataset *dset, 
                              GLfloat *tcorners, GLfloat *corners);
 


#endif
