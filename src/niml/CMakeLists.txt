# not using niml_fileread.c
add_library(
  niml OBJECT
  niml_b64.c
  niml_byteorder.c
  niml_dataset.c
  niml_do.c
  niml_dtable.c
  niml_element.c
  niml_elemio.c
  niml_header.c
  niml_htable.c
  niml_malloc.c
  niml_md5.c
  niml_private.h
  niml_registry.c
  niml_rowtype.c
  niml_stat.c
  niml_stream.c
  niml_struct.c
  niml_sucker.c
  niml_url.c
  niml_util.c
  niml_uuid.c
  niml_vector.c
)

if(COMP_MRILIB_MINI)
    target_compile_definitions(niml
        PUBLIC
        "MRILIB_MINI"
    )
endif()
set_target_properties(niml PROPERTIES PUBLIC_HEADER niml.h)
target_include_directories(
  niml INTERFACE $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
                 $<INSTALL_INTERFACE:${AFNI_INSTALL_INCLUDE_DIR}>
)

# f2c header is the only thing required but should probably use whatever f2c is
# on the system (or the one in the AFNI repo that is obtained by linking
# against the f2c build there.
target_link_libraries(niml PRIVATE f2c)

# Define replace xt that is not inherited from libheaders in the case of COMP_MRILIB_MINI
target_compile_definitions(niml
    PUBLIC
    "REPLACE_XT"
    )

add_library(AFNI::niml ALIAS niml)
