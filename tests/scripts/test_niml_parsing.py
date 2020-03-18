import xml.etree.ElementTree as ET
import nibabel as nb
from pathlib import Path
import subprocess as sp


from afnipy import niml_parsing
scans_dir = Path("AFNI_data6/afni")

data_paths = {
    "head_files": [x for x in scans_dir.glob("*.HEAD") if x.is_file()],
    "atlas_niml": "mini_data/AFNI_atlas_spaces.niml",
}


def get_afni_niml(fname):
    img = nb.load(fname)

    afni_extension = None
    for ext in img.header.extensions:
        if ext.get_code() == 4:
            afni_extension = ext

    if afni_extension is None:
        return None

    extension_niml = afni_extension.get_content()
    return extension_niml


def test_etree_to_dict():
    assert nml.etree_to_json(ET.fromstring("""<e/>""")) == {"e": None}
    assert nml.etree_to_json(ET.fromstring("""<e>text</e>""")) == {"e": "text"}
    assert nml.etree_to_json(ET.fromstring("""<e name="value" />""")) == {
        "e": {"@name": "value"}
    }
    assert nml.etree_to_json(ET.fromstring("""<e name="value">text</e>""")) == {
        "e": {"@name": "value", "#text": "text"}
    }
    assert nml.etree_to_json(ET.fromstring("""<e> <a>text</a> <b>text</b> </e>""")) == {
        "e": {"a": "text", "b": "text"}
    }
    assert nml.etree_to_json(ET.fromstring("""<e> <a>text</a> <a>text</a> </e>""")) == {
        "e": {"a": ["text", "text"]}
    }
    assert nml.etree_to_json(ET.fromstring("""<e> text <a>text</a> </e>""")) == {
        "e": {"#text": "text", "a": "text"}
    }


def test_atlas_niml_parsing(data):
    atlas_niml = nml.read_atlas_niml(data.atlas_niml)
    parsed_atlas_niml = nml.parse_dset_niml_json(
        nml.etree_to_json(ET.fromstring(atlas_niml))
    )


def test_afni_extension_parsing_in_head_converted(data):
    for fname in (x for x in data.head_files if x.exists()):
        fname = str(fname)

        sp.check_output(f"3dcopy {fname} -overwrite test.nii.gz", shell=True)

        extension_niml = get_afni_niml("test.nii.gz")

        parsed_nii_niml = nml.convert_afni_extension_niml(extension_niml)


# # Parse a nifti extension niml
# def test_afni_extension_parsing():

#     niis = [x for x in Path("afni_ci_test_data").glob("**/*nii.gz") if x.is_file()]
#     for fname in (x for x in niis if x.exists()):
#         fname = str(fname)

#         extension_niml = get_afni_niml(fname)

#         parsed_nii_niml = nml.convert_afni_extension_niml(extension_niml)
