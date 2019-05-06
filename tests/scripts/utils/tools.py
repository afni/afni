import filecmp
from pathlib import Path
import nibabel as nib  # type: ignore
import difflib
from . import misc
import tempfile
import itertools as IT
import os

from numpy.testing import assert_allclose  # type: ignore

from typing import Dict, List, NamedTuple


def assert_all_files_equal(
    data: NamedTuple,
    ignore_file_patterns: List = [],
    text_file_patterns: List = [],
    kwargs_1d: Dict = {},
    kwargs_log: Dict = {},
    kwargs_text_files: Dict = {},
    kwargs_scans: Dict = {"header_kwargs": {}, "data_kwargs": {}},
    kwargs_byte: Dict = {},
):
    """A convenience wrapping function to compare the output files of a test
    command. 1D files, log files, and files sppecified as text files are
    compared using custom assert logic. If nibabel can read other files as
    images their header data and numeric data is compared, otherwise the byte
    contents of the files are compared.

    Args:
        data: Fixture object used in tests

        ignore_file_patterns: List of substrings that if
        found in a filename marks it for exclusion

        text_file_patterns: List of substrings that if
        found in a filename marks it comparison using string diffing of its contents

        kwargs_...: Keyword arguments passed to
        the corresponding assert function.
    """
    comparison_dir = data.comparison_dir
    file_list = [
        f
        for f in data.outdir.glob("**/*")
        if f.is_file() and not any(pat in str(f) for pat in ignore_file_patterns)
    ]
    for fname in file_list:
        fname = Path(fname)
        # compare 1D files
        if fname.suffix == ".1D":
            assert_1dfiles_equal(comparison_dir, [fname], **kwargs_1d)
        elif fname.suffix == ".log":
            # compare logs
            assert_logs_equal(comparison_dir, [fname], **kwargs_log)
        elif any(pat in fname.name for pat in text_file_patterns):
            assert_textfiles_equal(comparison_dir, [fname], **kwargs_text_files)
        else:
            # Try to compare it as a scan
            try:
                assert_scans_equal(comparison_dir, [fname], **kwargs_scans)
            except nib.filebasedimages.ImageFileError:
                # Not sure how to treat this file so just do a byte comparison
                assert_files_by_byte_equal(comparison_dir, [fname], **kwargs_byte)


def assert_scans_equal(
    comparison_dir: Path, scans: List, header_kwargs: Dict = {}, data_kwargs: Dict = {}
):
    for fname in scans:
        equivalent_file = get_equivalent_file(comparison_dir, fname)
        image = nib.load(str(fname))
        equiv_image = nib.load(str(equivalent_file))
        _assert_scan_headers_equal(image.header, equiv_image.header, **header_kwargs)
        _assert_scan_data_equal(
            image.get_fdata(), equiv_image.get_fdata(), **data_kwargs
        )


def assert_files_by_byte_equal(comparison_dir: Path, file_list: List, **kwargs):
    """Compare list of files written as part of a test output with a
    pre-existing output directory

    Args:
        file_list : Description
        comparison_dir : Description
    """
    for fname in file_list:
        equivalent_file = get_equivalent_file(comparison_dir, fname)
        assert filecmp.cmp(fname, equivalent_file)


def assert_logs_equal(
    comparison_dir, flist, append_to_ignored=[], ignore_patterns=["AFNI version="]
):
    ignore_patterns = append_to_ignored + ignore_patterns
    assert_textfiles_equal(comparison_dir, flist, ignore_patterns=ignore_patterns)


def assert_textfiles_equal(
    comparison_dir, textfiles, old_has=None, new_has=None, ignore_patterns=[]
):
    for fname in textfiles:
        equivalent_file = get_equivalent_file(comparison_dir, fname)
        text_old = [
            x
            for x in Path(equivalent_file).read_text().splitlines()
            if not any(pat in x for pat in ignore_patterns)
        ]

        text_new = [
            x
            for x in Path(fname).read_text().splitlines()
            if not any(pat in x for pat in ignore_patterns)
        ]

        diff = difflib.unified_diff(text_old, text_new)
        diff_str = "\n".join(diff)
        assert diff_str == ""


def assert_1dfiles_equal(comparison_dir, file_list, fields=None, **all_close_kwargs):
    """Summary

    Args:
        comparison_dir (pathlib.Path): Directory path containing previous
        output that will be compared against.
        file_list (Iterable[Union[str,int]]): List of AFNI 1D files to compare.
        fields (list, optional): Only the matrix of numbers is comparied by
        default. Fields allows the user to specify a list containing the
        elements to compare. Possible values include: 'mat', 'name', 'fname',
        'aname', 'nvec', 'nt', 'tr', 'nrowfull', 'nruns', 'run_len',
        'run_len_nc', 'nroi', 'GLapplied', 'command', 'header', 'csimobj',
        'csimstat', 'havelabs', 'labels', 'groups', 'goodlist', 'runstart',
        'verb', 'ready', 'cormat', 'cosmat', 'cormat_ready', 'VO'
        rtol (float, optional): Used to set tolerance of matrix comparison
        atol (float, optional): Used to set tolerance of matrix comparison
    """
    if fields:
        raise NotImplementedError

    for fname in file_list:
        tool_1d = misc.try_to_import_afni_module("1d_tool")
        equivalent_file = get_equivalent_file(comparison_dir, fname)
        if not equivalent_file.exists():
            raise FileNotFoundError

        # Load 1D file data
        obj_1d = tool_1d.A1DInterface()
        obj_1d.init_from_file(fname)
        data = obj_1d.adata.__dict__["mat"]

        # Load template 1D file data
        obj_1d_equiv = tool_1d.A1DInterface()
        obj_1d_equiv.init_from_file(equivalent_file)
        data_equiv = obj_1d_equiv.adata.__dict__["mat"]

        # Test the data is equal
        assert_allclose(data, data_equiv, **all_close_kwargs)


def get_equivalent_file(comparison_dir, fname):
    fname = Path(fname)
    equivalent_file = comparison_dir / fname.relative_to(
        [p for p in fname.parents if p.name.startswith("output_2")][-1]
    )
    return equivalent_file


def _assert_scan_headers_equal(header_a, header_b, **kwargs):
    # tools.assert_image_headers_equal(
    #     [outfile], data.comparison_dir, test_list=[], ignore_list=[],
    assert header_a == header_b


def _assert_scan_data_equal(data_a, data_b, **kwargs):
    assert_allclose(data_a, data_b, **kwargs)


def _text_has(text, substrings):
    """Given a body of text and a list of substrings, raises an error if any
    substring does not exist.

    Args:
        text (str): Text to search through.
        substrings (list of str): A list of substrings to search for

    """
    for substr in substrings:
        assert substr in text


def _text_does_not_have(text, substrings):
    """Given a body of text and a list of substrings, raises an error if any
    substring does  exist.

    Args:
        text (str): Text to search through.
        substrings (list of str): A list of substrings to search for

    """
    for substr in substrings:
        assert substr not in text


def uniquify(path, sep="_"):
    # SO: questions/13852700/python-create-file-but-if-name-exists-add-number
    def name_sequence():
        count = IT.count()
        next(count)
        next(count)
        yield ""
        while True:
            yield "{s}{n:d}".format(s=sep, n=next(count))

    orig = tempfile._name_sequence
    with tempfile._once_lock:
        tempfile._name_sequence = name_sequence()
        path = os.path.normpath(path)
        dirname, basename = os.path.split(path)
        filename, ext = os.path.splitext(basename)
        fd, filename = tempfile.mkstemp(dir=dirname, prefix=filename, suffix=ext)
        tempfile._name_sequence = orig
    return Path(filename)
