import importlib
from pathlib import Path
import pytest
import collections
import afni_python.afni_base as ab

from afni_python.pipeline_utils import (check_for_valid_pipeline_dset,
                                        TemplateConfig, prepare_afni_output,
                                        get_test_data, run_check_afni_cmd,
                                        get_dict_diffs, ShellComFuture
                                        )

import pprint
import pickle


TEST_DIR, TEST_ANAT_FILE, PICKLE_PATH, *_ = get_test_data()


def get_class_obj(fully_qualified_path, *args, **kwargs):
    class_parts = fully_qualified_path.split('.')
    class_name = class_parts[-1]
    module = importlib.import_module('.'.join(class_parts[:-1]))
    class_ = getattr(module, class_name)
    return class_, class_name


def make_old_comparison(fully_qualified_path, *args, **kwargs):
    class_, class_name = get_class_obj(fully_qualified_path)
    out_pickle = PICKLE_PATH.with_name(class_name + '.pklz')
    if not out_pickle.exists():
        class_instance = class_(*args, **kwargs)
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
    o = prepare_afni_output(dset, suffix="_morphed")
    assert(o.path == dset.path)
    assert(o.prefix == dset.prefix + "_morphed")
    assert(o.view == dset.view)
    assert(o.out_prefix() != dset.out_prefix())
    assert(o.input() != dset.input())

    # Changing the view should work.
    dset_afni = ab.afni_name("test+orig.HEAD")
    o = prepare_afni_output(dset_afni, suffix="_morphed", view="+tlrc")
    assert(o.path == dset_afni.path)
    assert(o.prefix == dset_afni.prefix + "_morphed")
    assert(o.view == '+tlrc')
    assert(o.out_prefix() != dset_afni.out_prefix())

    # Although a nifti filename is forbidden the luxury of a view in pipelines.
    dset_afni = ab.afni_name("test+orig.nii.gz")
    with pytest.raises(ValueError):
        o = prepare_afni_output(dset_afni, suffix="_morphed", view="+tlrc")

    # o should be identical to afni_object (but with a different prefix etc.)
    output_mimic = ab.afni_name("test_morphed.nii.gz")
    o = prepare_afni_output(dset, suffix="_morphed")
    mimic_dict = {k: v for k, v in vars(output_mimic).items() if k != 'odir'}
    o_dict = {k: v for k, v in vars(o).items() if k != 'odir'}

    assert(mimic_dict == o_dict)


def setup_for_run_check_afni_cmd():
    # Setup objects for testing
    anat_scan = TEST_ANAT_FILE
    ps = TemplateConfig(anat_scan)
    message = "A default error message"
    dset = ab.afni_name(anat_scan)
    o = prepare_afni_output(dset, "_morphed")
    o.delete()
    return anat_scan, ps, message, dset, o

# @pytest.mark.skip(reason="no way of currently testing this")


def test_run_check_afni_cmd():
    # Setup objects for testing
    anat_scan, ps, message, dset, o = setup_for_run_check_afni_cmd()

    # raises error because this cannot be a dataset
    # as in datasets have certain file types...
    with pytest.raises(ValueError):
        bad_out_fname = anat_scan.with_name("output_path")
        bad_dset = ab.afni_name(bad_out_fname)
        o = prepare_afni_output(bad_dset, "_morphed")

    # Fails with cmd doing nothing useful
    with pytest.raises(RuntimeError):
        cmd_str_hello = "echo hello"
        run_check_afni_cmd(cmd_str_hello, ps, {'dset_1': o}, message)

    # Should run and return a valid output dset
    filepath = Path(o.path) / o.out_prefix()
    cmd_str = "touch {p}".format(p=filepath)
    out_dict = run_check_afni_cmd(cmd_str, ps, {'dset_1': o}, message)
    check_for_valid_pipeline_dset(out_dict['dset_1'])

    # Fails with pre-existing output
    with pytest.raises(RuntimeError):
        run_check_afni_cmd(cmd_str, ps, {'dset_1': o}, message)

    # Doesn't fail with ok_to_exist set
    ps.ok_to_exist = 1
    run_check_afni_cmd(cmd_str, ps, {'dset_1': o}, message)


def test_run_check_afni_cmd_with_stdout():
    # Setup objects for testing
    anat_scan, ps, message, dset, o = setup_for_run_check_afni_cmd()
    filepath = Path(o.path) / o.out_prefix()
    cmd_str = "touch {p}".format(p=filepath)

    # If stdout is required a shell_obj is passed into the function
    ps.oexec = "dry_run"
    shell_obj = ShellComFuture(cmd_str, eo=ps.oexec)

    # The command should be "executed" even for dry_run:
    in_dict = {'dset_1': o, "shell_obj": shell_obj}
    out_dict = run_check_afni_cmd(cmd_str, ps, in_dict, message)
    dry_run_shell_obj = out_dict['shell_obj']
    assert(dry_run_shell_obj.exc == 1)
    # Some default text should be recovered even with a dry run:
    assert(dry_run_shell_obj.future_text(0) == "Text from future")

    # With a real execution the stdout should be captured in this case nothing
    # was sent to stdout:
    ps.oexec = ""
    shell_obj = ShellComFuture(cmd_str, eo=ps.oexec)
    in_dict = {'dset_1': o, "shell_obj": shell_obj}
    out_dict = run_check_afni_cmd(cmd_str, ps, in_dict, message)
    out_shell_obj = out_dict['shell_obj']
    assert(out_shell_obj.exc == 1)
    assert(out_shell_obj.future_text(0) == None)

    # And if there is output...
    cmd_str = "echo hello"
    shell_obj = ShellComFuture(cmd_str, eo=ps.oexec)
    in_dict = {"shell_obj": shell_obj, "chdir": TEST_DIR}
    out_dict = run_check_afni_cmd(cmd_str, ps, in_dict, message)
    out_shell_obj = out_dict['shell_obj']
    assert(out_shell_obj.exc == 1)
    assert(out_shell_obj.future_text(0) == "hello")

    # Should fail if ps.oexec and shell_obj.eo diverge.
    with pytest.raises(ValueError):
        ps.oexec = "dry_run"
        out_dict = run_check_afni_cmd(cmd_str, ps, in_dict, message)


def test_TemplateConfig():
    instantiation = ["dask_template"]
    usr_conf_standard = make_old_comparison(
        "afni_python.pipeline_utils.TemplateConfig", *instantiation)
    usr_conf = TemplateConfig(*instantiation)

    # add in fields that were taken out:
    # usr_conf.do_center = 1

    print(pprint.pformat(get_dict_diffs(vars(usr_conf_standard), vars(usr_conf))))
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


# Test shell_com and ShellComFuture classes for some basic commands
@pytest.mark.parametrize("fully_qualified_classname",
                         ["afni_python.afni_base.shell_com", "afni_python.pipeline_utils.ShellComFuture"])
def test_ShellCom(fully_qualified_classname):
    # The ShellComFuture class should largely behave the same way except that it
    # can return output even if it has not been run
    cmd_str = "echo success"
    kwargs = {'capture': 1}
    old_com = make_old_comparison(fully_qualified_classname, cmd_str, **kwargs)
    ComClass_, name_ = get_class_obj(fully_qualified_classname)

    com = ComClass_(cmd_str, **kwargs)
    # this point the behavior of the com object can be probed.
    print(com)

    # accessing values before a run prints error but does not raise one.
    assert com.val(0) == None

    # ShellComFuture can access text output regardless of whether the command
    # has been executed yet. This is useful for using dry_run in pipelines.
    if name_ == "ShellComFuture":
        com.eo = "dry_run"
        assert(com.future_text(0) == "Text from future")
        com.eo = ""

    # com object should return stdout upon run:
    com.run()
    assert com.val(0) == "success"

    # ShellComFuture should now return text that was computed instead.
    if name_ == "ShellComFuture":
        assert(com.future_text(0) == "success")

# alternative approach to stdout capture
# from nipype.interfaces.base import CommandLine, CommandLineInputSpec,File,TraitedSpec, traits
# class ThreeDAttributeInputSpec(CommandLineInputSpec):
#     dset = File(exists=False, mandatory=True, argstr='%s',
#                    position=-1, desc='the input file')
#     aname = traits.String(desc = "stdout for the attribute", mandatory = True, position = -2, argstr = "%s")

# class ThreeDAttribute(CommandLine):
#     _cmd = '3dAttribute'
#     input_spec = ThreeDAttributeInputSpec
# test_ThreeDAttribute():

# result =ThreeDAttribute(
#     dset="/usr/local/apps/afni/current/linux_centos_7_64/MNI152_1mm_uni+tlrc.BRIK.gz",
#     aname = "DELTA").run()
