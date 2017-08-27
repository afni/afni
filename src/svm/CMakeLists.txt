set(SVM_SRCS 3dsvm_common.c svm_common.c svm_learn.c svm_hideo.c)

add_executable(3dsvm ${SVM_SRCS} 3dsvm.c)
add_library(plug_3dsvm ${SVM_SRCS} plug_3dsvm.c)

target_link_libraries(3dsvm mri)
target_link_libraries(plug_3dsvm mri AFNI::coxplot)

target_include_directories(3dsvm PRIVATE $<TARGET_PROPERTY:NIFTI::niftiio,INTERFACE_INCLUDE_DIRECTORIES>)

install(TARGETS 3dsvm plug_3dsvm
       RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
       LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
       ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
       PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
) 
