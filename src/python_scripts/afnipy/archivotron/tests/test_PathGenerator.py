from archivotron import PathGenerator

import os
import pytest


def test_constructor():
    """Makes sure the constructor works"""
    pg = PathGenerator("/data/tevesjb/my_project")
    assert pg is not None


def test_gen_path_succeeds():
    """Tests for gen_path successes"""
    pg = PathGenerator()
    # Build path
    pg.add_component("subject")
    pg.add_filesep()
    pg.add_component("session", required=False)
    pg.add_filesep()
    pg.add_component("modality", value_only=True)
    pg.add_filesep()
    pg.add_component("subject")
    pg.add_component("session", required=False)
    pg.add_component("submodality", value_only=True)
    pg.terminate()

    atts = {
        "modality": "anat",
        "submodality": "T1w"
    }

    expected = (
        "subject-Jen"
        "/anat"
        "/subject-Jen_T1w"
    )

    with pytest.raises(
        ValueError,
        match=r"^NameComponent is required for key *"
    ):
        pg.gen_path(atts)

    atts["subject"] = "Jen"
    assert pg.gen_path(atts) == expected

    atts["session"] = "pre"
    expected = (
        "subject-Jen"
        "/session-pre"
        "/anat"
        "/subject-Jen_session-pre_T1w"
    )
    assert pg.gen_path(atts) == expected

    af = PathGenerator(None, attribute_sep=".", kv_sep="")
    af.add_component("pb")
    af.add_component("subj")
    af.add_component("r")
    af.add_component("step", value_only=True)
    af.delimiter_override("+")
    af.add_component("space", value_only=True)
    af.terminate()

    atts = {
        "pb": "01",
        "subj": "99",
        "r": "01",
        "step": "tshift",
        "space": "orig",
    }

    expected = "pb01.subj99.r01.tshift+orig"

    assert af.gen_path(atts) == expected

    # BIDS T1w anatomical, func
    bids = PathGenerator()
    bids.add_component("sub")
    bids.add_filesep()
    bids.add_component("ses", required=False)
    bids.add_filesep()
    bids.add_component("modality", value_only=True)
    bids.add_filesep()
    bids.add_component("sub")
    bids.add_component("ses", required=False)
    bids.add_component("task", required=False)
    bids.add_component("acq", required=False)
    bids.add_component("ce", required=False)
    bids.add_component("rec", required=False)
    bids.add_component("run", required=False)
    bids.add_component("dir", required=False)
    bids.add_component("mod", required=False)
    bids.add_component("echo", required=False)
    bids.add_component("inv", required=False)
    bids.add_component("flip", required=False)
    bids.add_component("mt", required=False)
    bids.add_component("part", required=False)
    bids.add_component("recording", required=False)
    bids.add_component("suffix", value_only=True)
    bids.terminate()
    bids.add_inclusion_rule(
        "suffix", ["bold", "cbv", "sbref"],
        [
            "sub", "ses", "modality", "task", "acq", "ce", "rec", "dir",
            "run", "echo", "part", "suffix",
        ]
    )

    atts = {
        "sub": "01",
        "ses": "pre",
        "modality": "anat",
        "acq": "a",
        "ce": "a",
        "rec": "a",
        "run": "1",
        "part": "mag",
        "suffix": "T1w",
    }

    expected = (
        "sub-01/ses-pre/anat/sub-01_ses-pre_"
        "acq-a_ce-a_rec-a_run-1_part-mag_T1w"
    )

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

    with pytest.raises(ValueError, match=r"Rule violation*"):
        atts["recording"] = "resp"
        bids.gen_path(atts)

    with pytest.raises(ValueError, match=r"Rule violation*"):
        atts["recording"] = "resp"
        atts["suffix"] = "sbref"
        bids.gen_path(atts)


def test_gen_path_fails():
    """Tests for gen_path failures"""
    pg = PathGenerator("/")

    # Should fail because no path target
    with pytest.raises(ValueError, match=r"^No path target completed!*"):
        pg.gen_path({"subject": "Jen"})

    pg.add_component("subject")
    pg.terminate()


def test_json():
    # BIDS T1w anatomical, func
    bids = PathGenerator()
    bids.add_component("sub")
    bids.add_filesep()
    bids.add_component("ses", required=False)
    bids.add_filesep()
    bids.add_component("modality", value_only=True)
    bids.add_filesep()
    bids.add_component("sub")
    bids.add_component("ses", required=False)
    bids.add_component("task", required=False)
    bids.add_component("acq", required=False)
    bids.add_component("ce", required=False)
    bids.add_component("rec", required=False)
    bids.add_component("run", required=False)
    bids.add_component("dir", required=False)
    bids.add_component("mod", required=False)
    bids.add_component("echo", required=False)
    bids.add_component("inv", required=False)
    bids.add_component("flip", required=False)
    bids.add_component("mt", required=False)
    bids.add_component("part", required=False)
    bids.add_component("recording", required=False)
    bids.add_component("suffix", value_only=True)
    bids.terminate()
    bids.add_inclusion_rule(
        "suffix", ["bold", "cbv", "sbref"],
        [
            "sub", "ses", "modality", "task", "acq", "ce", "rec", "dir",
            "run", "echo", "part", "suffix",
        ]
    )

    atts = {
        "sub": "01",
        "ses": "pre",
        "modality": "anat",
        "acq": "a",
        "ce": "a",
        "rec": "a",
        "run": "1",
        "part": "mag",
        "suffix": "T1w",
    }

    temp_name = "TEMP_DELETE_ME.json"
    bids.to_json(temp_name)
    bids_from_file = PathGenerator.from_json(temp_name)
    os.remove(temp_name)

    assert bids.gen_path(atts) == bids_from_file.gen_path(atts)
