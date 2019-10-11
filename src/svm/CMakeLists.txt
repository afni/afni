set(SVM_SRCS 3dsvm_common.c svm_common.c svm_learn.c svm_hideo.c)

add_executable(3dsvm ${SVM_SRCS} 3dsvm.c)
add_library(plug_3dsvm ${SVM_SRCS} plug_3dsvm.c)

target_include_directories(3dsvm 
  PRIVATE 
    $<TARGET_PROPERTY:NIFTI::niftiio,INTERFACE_INCLUDE_DIRECTORIES> 
    $<TARGET_PROPERTY:libheaders,INTERFACE_INCLUDE_DIRECTORIES>
    )
# target_include_directories(plug_3dsvm
#   PRIVATE 
#     $<TARGET_PROPERTY:libheaders,INTERFACE_INCLUDE_DIRECTORIES>
#     )

target_link_libraries(3dsvm PRIVATE  mrix mri libheaders m)
target_link_libraries(plug_3dsvm PRIVATE mrix mri AFNI::coxplot libheaders)

install(TARGETS 3dsvm plug_3dsvm
       RUNTIME DESTINATION ${AFNI_INSTALL_RUNTIME_DIR}
       LIBRARY DESTINATION ${AFNI_INSTALL_LIBRARY_DIR}
       ARCHIVE DESTINATION ${AFNI_INSTALL_LIBRARY_DIR}
       PUBLIC_HEADER DESTINATION ${AFNI_INSTALL_INCLUDE_DIR}
) 
