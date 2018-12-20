add_executable(parent_exe parent_project_exe.c)
target_link_libraries(parent_exe PUBLIC ${NIFTI_PACKAGE_PREFIX}nifticdf)
