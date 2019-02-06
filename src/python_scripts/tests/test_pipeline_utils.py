from pathlib import Path
import pytest
import collections
import afni_python.afni_base as ab

from afni_python.pipeline_utils import ( check_for_valid_pipeline_dset,
    TemplateConfig, prepare_afni_output, get_test_data, run_check_afni_cmd,
    get_dict_diffs)

import pprint 
import pickle


TEST_DIR, TEST_ANAT_FILE, PICKLE_PATH, *_ = get_test_data()


# with PICKLE_PATH.open('wb') as f:
#     usr_conf = TemplateConfig("dask_template")
#     pickle.dump(usr_conf, f)

def make_old_comparison(in_class,args,**kwargs):
    class_name = str(in_class.__class__).split("'")[-2]
    out_pickle = PICKLE_PATH.with_name(class_name + '.pklz')
    if not out_pickle.exists():
        class_instance = in_class(*args,**kwargs)
        with out_pickle.open('wb') as f:
            pickle.dump(class_instance, f)
    else:
        class_instance = pickle.load(out_pickle.open('rb'))
    return class_instance
    

def test_prepare_afni_output():
    """
    Should return an object that is identical to one from afni_name with the
    same prefix, with the important difference that the latter MUST have an
    extension identical. The view may be changed. The prefix should be
    changed. The prefix should not have extensions or view placed after it.
    """

    # Basic expectation of output object attributes
    dset = ab.afni_name("test.nii.gz")
    o = prepare_afni_output(dset,suffix= "_morphed")
    assert(o.path == dset.path)
    assert(o.prefix == dset.prefix + "_morphed")
    assert(o.view == dset.view)
    assert(o.out_prefix() != dset.out_prefix())
    assert(o.input() != dset.input())

    # Changing the view should work.
    dset_afni = ab.afni_name("test+orig.HEAD")
    o = prepare_afni_output(dset_afni,suffix= "_morphed",view = "+tlrc")
    assert(o.path == dset_afni.path)
    assert(o.prefix == dset_afni.prefix + "_morphed")
    assert(o.view == '+tlrc')
    assert(o.out_prefix() != dset_afni.out_prefix())

    # Although a nifti filename is forbidden the luxury of a view in pipelines.
    dset_afni = ab.afni_name("test+orig.nii.gz")
    with pytest.raises(ValueError):
        o = prepare_afni_output(dset_afni,suffix= "_morphed",view = "+tlrc")
    

    # o should be identical to afni_object (but with a different prefix etc.)
    output_mimic = ab.afni_name("test_morphed.nii.gz")
    o = prepare_afni_output(dset,suffix= "_morphed")
    mimic_dict = {k:v for k,v in vars(output_mimic).items() if k != 'odir'}
    o_dict = {k:v for k,v in vars(o).items() if k != 'odir'}

    assert(mimic_dict == o_dict)




def test_mock_run_check_afni_cmd():
    # Setup objects for testing
    anat_scan = TEST_ANAT_FILE
    ps = TemplateConfig(anat_scan)
    message = "A default error message"
    
    # raises error because this cannot be a dataset
    # as in datasets have certain file types...
    with pytest.raises(ValueError):
        out_fname = anat_scan.with_name("output_path")
        dset = ab.afni_name(out_fname)
        o = prepare_afni_output( dset, "_morphed")
        # filepath = Path(o.path) / o.out_prefix()
        # cmd_str =  "touch {p}".format(p=filepath)
        # run_check_afni_cmd(cmd_str, ps, o, message)


    # Should run and return a valid output dset
    dset = ab.afni_name(anat_scan)
    o = prepare_afni_output( dset, "_morphed")
    o.delete()
    filepath = Path(o.path) / o.out_prefix()
    cmd_str =  "touch {p}".format(p=filepath)
    run_check_afni_cmd(cmd_str, ps, o, message)
    check_for_valid_pipeline_dset(o)
    

    # Fails with pre-existing output
    with pytest.raises(ValueError):
        run_check_afni_cmd(cmd_str, ps, o, message)

    # Doesn't fail with ok_to_exist set
    ps.ok_to_exist = 1
    run_check_afni_cmd(cmd_str, ps,o, message)


    # Fails with cmd doing nothing useful
    o.delete()
    cmd_str = "echo hello".format(f=out_fname)
    with pytest.raises(RuntimeError):
        run_check_afni_cmd(cmd_str, ps, o, message)


def test_TemplateConfig():
    usr_conf_standard = pickle.load(PICKLE_PATH.open('rb'))
    usr_conf = TemplateConfig("dask_template")

    # add in fields that were taken out:
    usr_conf.do_center = 1

    print(pprint.pformat( get_dict_diffs(vars(usr_conf_standard), vars(usr_conf))))
    assert(usr_conf_standard == usr_conf)


def test_check_for_valid_pipeline_dset():
    
    # dset suffix must be supplied
    dset = ab.afni_name('output_path')
    with pytest.raises(ValueError):
        check_for_valid_pipeline_dset(dset)

    # BRIK extension should not be used.
    # not using this for now
    # dset = ab.afni_name('output_path.BRIK')
    # with pytest.raises(ValueError):
    #     check_for_valid_pipeline_dset(dset)

    # # If not HEAD or NIFTI it should fail
    # dset = ab.afni_name('output_path.1d')
    # with pytest.raises(ValueError):
    #     check_for_valid_pipeline_dset(dset)

    # example use for nifti:
    dset = ab.afni_name('output_path.nii')
    check_for_valid_pipeline_dset(dset)

    dset = ab.afni_name('output_path.nii.gz')
    check_for_valid_pipeline_dset(dset)

    dset = ab.afni_name('output_path+tlrc.BRIK')
    check_for_valid_pipeline_dset(dset)

    # Fails for BRIK without a view
    dset = ab.afni_name('output_path.BRIK')
    with pytest.raises(ValueError):
        check_for_valid_pipeline_dset(dset)

    # Fails for NIFTI *with* a view
    dset = ab.afni_name('output_path+tlrc.nii')
    check_for_valid_pipeline_dset(dset)

    # Passes for HEAD with a view set manually
    dset = ab.afni_name('output_path.HEAD')
    dset.view = '+tlrc'
    check_for_valid_pipeline_dset(dset)