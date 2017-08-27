add_library(nifti2_io nifti2_io.c)

set_target_properties(nifti2_io PROPERTIES
	PUBLIC_HEADER nifti2_io.h
)
target_include_directories(nifti2_io PUBLIC ${NIFTI_INCLUDE_DIRS})
