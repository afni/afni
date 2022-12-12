import warnings

import pytest

from archivotron.bids import generate_bids


def test_generate_bids():
    """Tests for the bids generator; cloned from the other test"""
    bids = generate_bids()

    atts = {
        "sub": "01",
        "ses": "pre",
        "modality": "func",
        "task": "rest",
        "acq": "a",
        "ce": "a",
        "rec": "a",
        "dir": "PA",
        "run": "1",
        "echo": "1",
        "part": "mag",
        "suffix": "bold",
    }

    expected = (
        "sub-01/ses-pre/func/sub-01_ses-pre_"
        "task-rest_acq-a_ce-a_rec-a_run-1_dir-PA_echo-1_part-mag_bold"
    )

    assert bids.gen_path(atts) == expected


def test_bids_into():
    """Tests for breaking bids names into attributes"""
    bids = generate_bids()

    # simple anatomical

    expected = {
        "sub": "1",
        "modality": "anat",
        "suffix": "T1w",
    }
    p = "/sub-1/anat/sub-1_T1w"
    assert bids.into_attributes(p) == expected

    expected = {
        "sub": "01",
        "ses": "pre",
        "modality": "func",
        "task": "scream",
        "echo": "1",
        "suffix": "bold",
    }
    p = "/sub-01/ses-pre/func/sub-01_ses-pre_task-scream_echo-1_bold"
    assert bids.into_attributes(p) == expected

    # This will trigger a warning
    with pytest.warns(UserWarning):
        expected = {"sub": "1", "suffix": "T1w"}
        p = "/sub-1_T1w"
        assert bids.into_attributes(p) == expected

    # This should print nothing at all
    with warnings.catch_warnings():
        warnings.simplefilter("error")
        expected = {"sub": "1", "suffix": "T1w"}
        p = "/sub-1_T1w"
        assert bids.into_attributes(p, mode="loose") == expected

    with pytest.raises(ValueError, match=r"Required key"):
        p = "/sub-1_T1w"
        assert bids.into_attributes(p, mode="strict") == expected

    with pytest.raises(ValueError, match=r"Mode"):
        p = "/sub-01/ses-pre/func/sub-01_ses-pre_task-scream_echo-1_bold"
        bids.into_attributes(p, mode="Modey McModeFace")
