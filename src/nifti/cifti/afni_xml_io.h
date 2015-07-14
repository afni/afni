#ifndef AFNI_XML_IO_H
#define AFNI_XML_IO_H


#include <stdio.h>
#include "afni_xml.h"

/* ----------------------------------------------------------------------
   CIFTI XML structure:

      CIFTI
            attr = Version
         Matrix
            MetaData
               MD
                  Name [text]
                  Value [text]
            MatrixIndicesMap
                  attr = ApliesToMatrixDimension,
                         IndicesMapToDataType [int = intent code],
                         NumberOfSeriesPoints, SeriesExponenet,
                         SeriesStart, SeriesStep, SeriesUnit [string??]
               NamedMap
                  MetaData
                  LabelTable
                     Label [text - name of label]
                        attr = Key[int], Red, Green, Blue, Alpha [f 0..1]
                  MapName [text - name of map]
               Surface
                  attr = BrainStructure, SurfaceNumberOfVertices [int64_t]
               Parcel
                  attr = Name [text]
                  Vertices [int64_t (of unknown length?)]
                     attr = BrainStructure
                  VoxelIndicesIJK
               Volume
                  attr = VolumeDimensions [int64_t,int64_t,int64_t]
                  TransformationMatrixVoxelIndicesIJKtoXYZ
                     [text - 16 x double xform matrix (row major)]
                     attr = MeterExponent [int - power of 10]
               BrainModel
                  attr = IndexOffset [int], IndexCount [int64_t],
                         ModelType[??], BrainStructure[??],
                         SurfaceNumberOfVertices
                  VoxelIndicesIJK [int64_t triples (see IndexCount)]
                     * might convert IJK to just Indices via VolumeDimensions
                       (this is only for volume, Surface gives just indices)
                  VertexIndices [int64_t (see IndexCount)]

    - convert as in SUMA_Create_Fake_CIFTI()
 * ----------------------------------------------------------------------*/

/* --------------------------- structures --------------------------------- */



/* --------------------------- prototypes --------------------------------- */

int axio_read_cifti_file(const char * fname, int get_ndata, 
                         nifti_image ** nim_out, afni_xml_t ** ax_out);

afni_xml_t * axio_read_buf (const char * buf, int64_t blen);
afni_xml_t * axio_read_file(const char * fname);

int axio_text_to_binary (afni_xml_t * ax);
int axio_num_tokens     (const char * str, int64_t maxlen);

#endif /* AFNI_XML_IO_H */
