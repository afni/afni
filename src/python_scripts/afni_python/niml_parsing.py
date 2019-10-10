import xml.etree.ElementTree as ET

from pathlib import Path
from collections import defaultdict
import subprocess as sp


def etree_to_json(t):
    """Implementation of:
                http://www.xml.com/pub/a/2006/05/31/converting-between-xml-and-json.html
                From:
                https://stackoverflow.com/questions/2148119/how-to-convert-an-xml-string-to-a-dictionary
            """
    d = {t.tag: {} if t.attrib else None}
    children = list(t)
    if children:
        dd = defaultdict(list)
        for dc in map(etree_to_json, children):
            for k, v in dc.items():
                dd[k].append(v)
        d = {t.tag: {k: v[0] if len(v) == 1 else v for k, v in dd.items()}}
    if t.attrib:
        d[t.tag].update(("@" + k, v) for k, v in t.attrib.items())
    if t.text:
        text = t.text.strip()
        if children or t.attrib:
            if text:
                d[t.tag]["#text"] = text
        else:
            d[t.tag] = text
    return d


def convert_niml_to_json(niml):
    niml_tree = ET.fromstring(niml)
    return etree_to_json(niml_tree)


def parse_dset_niml_json(niml_json, remove_attr_tag=False):
    if type(niml_json) == list:
        return [parse_dset_niml_json(x, remove_attr_tag) for x in niml_json]

    # if len(niml_json.keys()) == 1:
    #     return parse_dset_niml_json(niml_json[list(niml_json.keys())[0]], remove_attr_tag)

    output = {}
    if "AFNI_atr" in niml_json.keys():
        output["AFNI_atr"] = parse_afni_attributes(niml_json.pop("AFNI_atr"))

    for key in niml_json.keys():
        if key.startswith("@"):
            if remove_attr_tag:
                output[key.strip("@")] = niml_json[key]
            else:
                output[key] = niml_json[key]

        elif key.startswith("#"):
            value = niml_json[key]
            assert not value.startswith("<")
            output[key] = value

        else:
            value = parse_dset_niml_json(niml_json[key], remove_attr_tag)
            output[key] = value

    return output


def parse_afni_attributes(niml_json):
    type_mapping = {"int": int, "float": float, "String": str}

    parsed_attrs = {}
    for attribute in niml_json:
        attr_type = type_mapping[attribute["@ni_type"]]
        attr_name = attribute["@atr_name"]
        attr_count = attribute["@ni_dimen"]
        attr_val = attribute["#text"].strip('\n "').replace('"\n "', "")

        if attr_type is not str:
            parsed_attr = [attr_type(f) for f in attr_val.split()]
        elif attr_name in ["VALUE_LABEL_DTABLE", "ATLAS_LABEL_TABLE"]:
            continue
        elif attr_val.startswith("<"):
            niml_subtree = ET.fromstring(attr_val)
            parsed_attr = parse_dset_niml_json(
                etree_to_json(niml_subtree), remove_attr_tag=True
            )
        else:
            # AFNI string attributes will always start with open single quote and
            # end with a tilde (NUL). These attributes CANNOT contain tildes (so
            # stripping is safe), but can contain single quotes (so we replace)
            parsed_attr = attr_val.replace("'", "", 1).rstrip("~")

        parsed_attrs[attr_name] = (
            parsed_attr[0] if len(parsed_attr) == 1 else parsed_attr
        )

    return parsed_attrs


def convert_afni_extension_niml(extension_niml, remove_attr_tag=False):
    if extension_niml is None:
        return None

    niml_dset_json = convert_niml_to_json(extension_niml)
    dset_info = parse_dset_niml_json(niml_dset_json)
    return dset_info


def read_atlas_niml(fname):
    from pathlib import Path
    import re

    niml_txt = Path(fname).read_text()
    niml_cleaned_txt = "\n".join(
        [x for x in niml_txt.splitlines() if not (x.startswith("#") or x == "")]
    )
    niml_cleaned_txt = niml_cleaned_txt.replace("&", "and")

    p = re.compile('(\\\\n"\n( *)")')
    niml_cleaned_txt = p.sub("(?2)", niml_cleaned_txt)
    niml_cleaned_txt = niml_cleaned_txt.replace('\\n"', '"')
    # print('\n'.join(niml_cleaned_txt.splitlines()[0:20]))
    return "<doc> \n" + niml_cleaned_txt + "</doc>"
