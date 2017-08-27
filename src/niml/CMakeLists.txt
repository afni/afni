cmake_minimum_required(VERSION 3.1...3.12)

# not using niml_fileread.c
add_library(niml OBJECT niml_b64.c niml_byteorder.c niml_dataset.c niml_do.c niml_dtable.c
niml_element.c niml_elemio.c niml_header.c niml_htable.c niml_malloc.c
niml_md5.c niml_private.h niml_registry.c niml_rowtype.c niml_stat.c
niml_stream.c niml_struct.c niml_sucker.c niml_url.c niml_util.c niml_uuid.c
niml_vector.c)

set_target_properties(niml PROPERTIES PUBLIC_HEADER ../niml.h )
target_link_libraries(niml libheaders) 
add_library(AFNI::niml ALIAS niml)
