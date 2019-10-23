set(SVM_SRCS 3dsvm_common.c svm_common.c svm_learn.c svm_hideo.c)

add_executable(3dsvm ${SVM_SRCS} 3dsvm.c)

target_include_directories(
  3dsvm PRIVATE $<TARGET_PROPERTY:NIFTI::niftiio,INTERFACE_INCLUDE_DIRECTORIES>
                $<TARGET_PROPERTY:libheaders,INTERFACE_INCLUDE_DIRECTORIES>
)

target_link_libraries(3dsvm PRIVATE AFNI::mrix AFNI::mri libheaders m)
