import importlib
import os
from pathlib import Path
import pytest
import collections
import afni_python.afni_base as ab

from afni_python.pipeline_utils import (TemplateConfig, prepare_afni_output,
                                        get_test_data, run_check_afni_cmd,
                                        get_dict_diffs, ShellComFuture, working_directory,
                                        change_to_afni, change_to_nifti, make_nii_compatible)

import pprint
import pickle


TEST_DIR, TEST_ANAT_FILE, PICKLE_PATH, *_ = get_test_data()
os.chdir(TEST_DIR)


def test_change_to_afni():
    anat_scan, ps, message, dset, o = setup_for_run_check_afni_cmd()


def test_make_nii_compatible():
    anat_scan, ps, message, dset, o = setup_for_run_check_afni_cmd()
    ps.ok_to_exist = True

    @make_nii_compatible(mod_params={'args_in': [0, ], 'ret_vals': [0]}, config_name='ps')
    def fail_if_not_brik(dset, arg_arb, x=None, ps=None):
        assert dset.type == "BRIK"
        assert dset.exist()
        return dset

    @make_nii_compatible(mod_params={'args_in': [0, ], 'ret_vals': [0]}, config_name='ps')
    def add_suffix_to_brik(dset, ps=None, x=None):
        assert dset.type == "BRIK"
        assert dset.exist()
        o = prepare_afni_output(dset, suffix="a_suffix")
        cmd_str = "3dcopy %s %s" % (dset.initname, o.initname)
        run_check_afni_cmd(cmd_str, ps, {'dset_1': o})
        return dset

    # Check BRIK is passed in and has been appropriately copied
    print(dset)
    ret_val = fail_if_not_brik(dset, "", x="tree", ps=ps)

    # ps must be passed as a keyword object
    with pytest.raises(ValueError):
        ret_val = fail_if_not_brik(dset, "", x="tree")

    # wrapper checks call signature and makes sure there are n positional args
    # specified
    # with pytest.raises(ValueError):
    #     ret_val = fail_if_not_brik(dset,arg_arb = "",x = "tree",ps=ps)

    # Check that when an output is made that the nifti extension exists
    out_dset = add_suffix_to_brik(dset, ps=ps, x="tree")
    assert(out_dset.extension == ".nii.gz")
    assert(out_dset.exist())


def test_check_for_strict_name():
    # Fails for NIFTI *with* a view
    with pytest.raises(ValueError):
        ab.check_for_strict_name("test+tlrc.nii.gz")
    with pytest.raises(ValueError):
        # dset extension must be supplied
        ab.check_for_strict_name("test")
    with pytest.raises(ValueError):
        ab.check_for_strict_name("test.HEAD")
    with pytest.raises(ValueError):
        # Fails for BRIK without a view
        ab.check_for_strict_name("test.BRIK")
    with pytest.raises(ValueError):
        ab.check_for_strict_name("/Users/rodgersleejg/test+tlrc.BRIK")
    # # If not HEAD or NIFTI it should fail
    # dset = ab.afni_name('output_path.1d')
    # with pytest.raises(ValueError):
    #     check_for_strict_name(dset)
    # Working examples
    ab.check_for_strict_name("test.nii.gz")
    ab.check_for_strict_name("test+tlrc.BRIK")


def test_afni_name():
    dset = ab.afni_name("test.nii.gz")
    assert(dset.initname == 'test.nii.gz')
    assert(dset.path == str(Path.cwd()) + '/')
    assert(dset.prefix == 'test')
    assert(dset.view == '')

    # view/prefix/extension behaves puzzlingly on nifti. Use bn, fn,
    # initpath, and initname with strict=True
    dset = ab.afni_name("test+tlrc.nii.gz")
    assert(dset.view == '')
    assert(dset.prefix == 'test+tlrc')
    assert(dset.rpve() == "test+tlrc.nii.gz")
    assert(dset.rpv() == "test+tlrc.nii.gz")
    with pytest.raises(ValueError):
        dset = ab.afni_name("test+tlrc.nii.gz", strict=True)

    # Can change initname
    dset.initname = "booga.nii.gz"

    # filename attributes with a relative directory
    init_basename_wd = "test/test/test"
    dset_wd = ab.afni_name(init_basename_wd + ".nii.gz")
    dset_wd_brik = ab.afni_name(init_basename_wd + "+tlrc.BRIK")
    assert(dset_wd.initpath == os.getcwd())
    assert(dset_wd_brik.initpath == os.getcwd())
    with working_directory('/tmp'):
        assert(dset_wd.initpath != os.getcwd())
        assert(dset_wd_brik.initpath != os.getcwd())
    assert(dset_wd.initname == init_basename_wd + '.nii.gz')
    assert(dset_wd_brik.initname == init_basename_wd + '+tlrc.BRIK')
    assert(dset_wd.prefix == 'test')
    assert(dset_wd_brik.prefix == 'test')

    # Test effects of moving around directory
    with working_directory('/'):
        dset_old = ab.afni_name(TEST_ANAT_FILE.relative_to('/'))
        assert(dset_old.initname == str(TEST_ANAT_FILE.relative_to('/')))
        assert(dset_old.ppve() == str(TEST_ANAT_FILE.parent / dset_old.pve()))
        assert(dset_old.initpath + dset_old.initname == str(TEST_ANAT_FILE))
        assert(dset_old.ppve() == dset_old.p() +
               dset_old.prefix + dset_old.view + dset_old.extension)
    assert(dset_old.initname == str(TEST_ANAT_FILE.relative_to('/')))
    assert(dset_old.ppve() == str(TEST_ANAT_FILE.parent.resolve() / dset_old.pve()))
    assert(dset_old.ppve() == dset_old.p() +
           dset_old.prefix + dset_old.view + dset_old.extension)
    assert(dset_old.initpath == '/')

    # ppve robust
    with working_directory('/'):
        assert(dset_old.ppve() == str(TEST_ANAT_FILE))
    assert(dset_old.ppve() == str(TEST_ANAT_FILE.resolve()))

    # A new object instance will inherit path but not initpath
    with working_directory('/tmp'):
        dset_spawned = dset.new("test.nii.gz")
        assert(dset.initpath != dset_spawned.initpath)
        assert(dset.path == dset_spawned.path)


def test_afni_name_strict():
    # Preserving pre-existing behavior
    dset = ab.afni_name("test.nii.gz", strict=True)
    assert(dset.view == '')
    assert(dset.prefix == 'test')
    assert(dset.rpve() == "test.nii.gz")
    assert(dset.rpv() == "test.nii.gz")

    # Desired behavior
    # Cannot modify initname
    with pytest.raises(ValueError):
        dset.initname = "booga.nii.gz"

    # For nifti:
    assert(dset.initname == "test.nii.gz")
    assert(dset.view == '')
    assert(dset.bn == "test")
    assert(dset.fn == "test.nii.gz")
    # For BRIK
    with pytest.raises(ValueError):
        dset = ab.afni_name("test.BRIK", strict=True)
    dset_brik = dset.new("test+tlrc.BRIK", strict=True)
    assert(dset_brik.initname == "test+tlrc.BRIK")
    assert(dset_brik.view == '+tlrc')
    assert(dset_brik.bn == "test")
    assert(dset_brik.fn == "test+tlrc.BRIK")

    # filename attributes with a relative directory
    init_basename_wd = "test/test/test"
    dset_wd = ab.afni_name(init_basename_wd + ".nii.gz", strict=True)
    dset_wd_brik = ab.afni_name(init_basename_wd + "+tlrc.BRIK", strict=True)
    assert(dset_wd.initname == init_basename_wd + '.nii.gz')
    assert(dset_wd_brik.initname == init_basename_wd + '+tlrc.BRIK')
    assert(dset_wd.prefix == 'test')
    assert(dset_wd_brik.prefix == 'test')
    assert(dset_wd.bn == dset_wd.prefix)  # for unedited objects
    assert(dset_wd_brik.bn == dset_wd.prefix)
    assert(dset_wd.fn == dset_wd.prefix + dset_wd.view + dset_wd.extension)
    assert(dset_wd_brik.fn == dset_wd_brik.prefix +
           dset_wd_brik.view + dset_wd_brik.extension)

    # Relative directory behavior
    assert(dset_wd.rel_dir() + dset_wd.prefix == init_basename_wd)
    assert(dset_wd_brik.rel_dir() + dset_wd.prefix == init_basename_wd)


    # A new object instance will inherit path and, unlike unstrict objects,
    # initpath
    with working_directory('/tmp'):
        dset_spawned = dset.new("test.nii.gz")
        assert(dset.initpath == dset_spawned.initpath)
        assert(dset.path == dset_spawned.path)

    # rbn returns relative path and the base name
    assert(dset.rbn == str(Path(dset.initname).parent / dset.bn))
    with working_directory('/tmp'):
        assert(dset.rbn == str(Path(dset.initname).parent / dset.bn))

    # initname preserves relpath for strict dataset: not yet implemented
    # dset = ab.afni_name("test/test/test.nii.gz")
    # assert(dset.initname == 'test/test/test.nii.gz')
    # # and reldir works, nope this will be kept the same:
    # assert(dset.initname == 'test/test/test.nii.gz')


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
    Should return an object that is identical to one from ab.afni_name with the
    same prefix, with the important difference that the latter MUST have an
    extension identical. The view may be changed. The prefix should be
    changed. The prefix should not have extensions or view placed after it.
    """
    with working_directory(TEST_DIR):
        # Basic expectation of output object attributes
        dset = ab.afni_name("test.nii.gz", strict=True)
        o = prepare_afni_output(dset, suffix="_morphed")
        assert(o.path == dset.path)
        assert(o.prefix == dset.prefix + "_morphed")
        assert(o.view == dset.view)
        assert(o.out_prefix() != dset.out_prefix())
        assert(o.input() != dset.input())

        # Changing the view should work.
        dset_afni = ab.afni_name("test+orig.BRIK", strict=True)
        o = prepare_afni_output(dset_afni, suffix="_morphed", view="+tlrc")
        assert(o.path == dset_afni.path)
        assert(o.prefix == dset_afni.prefix + "_morphed")
        assert(o.view == '+tlrc')
        assert(o.out_prefix() != dset_afni.out_prefix())

        # Changing the path should work
        dset_afni = ab.afni_name("first_dir/test+orig.BRIK", strict=True)
        o = prepare_afni_output(dset_afni, suffix="_morphed", path='second_dir')
        assert(o.path == str(TEST_DIR.resolve() / "second_dir") + '/')

        # nifti filename is forbidden the luxury of a view in strict datasets.
        dset_afni = ab.afni_name("test+orig.nii.gz")
        with pytest.raises(ValueError):
            o = prepare_afni_output(dset_afni, suffix="_morphed", view="+tlrc")

        # o should be identical to afni_object (but with a different prefix etc.)
        output_mimic = ab.afni_name("test_morphed.nii.gz", strict=True)
        o = prepare_afni_output(dset, suffix="_morphed")
        mimic_dict = {k: v for k, v in vars(
            output_mimic).items() if k != 'odir'}
        o_dict = {k: v for k, v in vars(o).items() if k != 'odir'}

        assert(mimic_dict == o_dict)


def setup_for_run_check_afni_cmd():
    # Setup objects for testing
    anat_scan = Path(TEST_ANAT_FILE).relative_to(TEST_DIR)
    ps = TemplateConfig(anat_scan)
    message = "A default error message"
    dset = ab.afni_name(str(anat_scan), strict=True)
    o = prepare_afni_output(dset, "_morphed")
    o.delete()
    return anat_scan, ps, message, dset, o

# @pytest.mark.skip(reason="no way of currently testing this")


def test_run_check_afni_cmd():
    # Setup objects for testing
    anat_scan, ps, message, dset, o = setup_for_run_check_afni_cmd()
    with Path(TEST_DIR):
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
        out_dict['dset_1'].is_strict

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
