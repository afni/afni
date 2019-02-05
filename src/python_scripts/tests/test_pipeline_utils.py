import pytest
from afni_python.pipeline_utils import check_for_valid_pipeline_dset
import afni_python.afni_base as ab

def test_check_for_valid_pipeline_dset():
    
    # dset suffix must be supplied
    dset = ab.afni_name('output_path')
    with pytest.raises(ValueError):
        check_for_valid_pipeline_dset(dset)

    # BRIK extension should not be used.
    dset = ab.afni_name('output_path.BRIK')
    with pytest.raises(ValueError):
        check_for_valid_pipeline_dset(dset)

    # If not HEAD or NIFTI it should fail
    dset = ab.afni_name('output_path.1d')
    with pytest.raises(ValueError):
        check_for_valid_pipeline_dset(dset)

    # passes for head or nifti:
    dset = ab.afni_name('output_path.nii')
    check_for_valid_pipeline_dset(dset)

    dset = ab.afni_name('output_path.nii.gz')
    check_for_valid_pipeline_dset(dset)

    dset = ab.afni_name('output_path+tlrc.HEAD')
    check_for_valid_pipeline_dset(dset)

    # Fails for HEAD without a view
    dset = ab.afni_name('output_path.HEAD')
    with pytest.raises(ValueError):
        check_for_valid_pipeline_dset(dset)

    # Passes if view subsequently set for HEAD without a view
    dset = ab.afni_name('output_path.HEAD')
    dset.view = '+tlrc'
    check_for_valid_pipeline_dset(dset)
    