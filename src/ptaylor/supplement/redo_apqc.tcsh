#!/bin/tcsh
 
# move any existing QC_* dir, and build another

set here = $PWD

set go_to_dir = ( ${argv[1-]} )

echo "++ REDOing APQC"
if ( "${go_to_dir}" == "" ) then
    echo "++ Going to re-run APQC in this dir only: ${here}"
    set go_to_dir = ( . )    # a "list" of just this dir
else
    echo "++ Going to re-run APQC in this many dirs: ${#go_to_dir}"
endif


foreach dd ( ${go_to_dir} ) 

    cd ${here}
    cd ${dd}

    set pre_qc = `find . -maxdepth 1 -type d -name "QC*"`

    if ( "${pre_qc}" != "" ) then
        set pre_qc_name = `echo ${pre_qc} | cut -b3-`
        set thedate = `date +%Y_%m_%d_%H_%M_%s`
        \mv ${pre_qc} old_${pre_qc_name}_${thedate}
    endif

    apqc_make_tcsh.py -review_style pythonic -subj_dir . \
        -uvar_json out.ss_review_uvars.json
    tcsh @ss_review_html |& tee out.review_html

    set now_qc = `find . -maxdepth 1 -type d -name "QC*"`
    apqc_make_html.py -qc_dir ${now_qc}
end
