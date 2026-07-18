"""PathGenerator, contains definition for PathGenerator class"""

from typing import Union
from warnings import warn

import os
import json


# This is hacky sorry
DIRECTORY_KEYWORD = "DIRECTORY_SPECIAL_KEYWORD"


class PathGenerator:
    """A class to generate paths from specified attributes

    Attributes
    ----------
    _attributes: dict
        A dictionary of various attributes and their properties
    _components: list(NameComponent)
        A list of naming components
    _terminated: bool
        Whether this object's builder pattern has terminated
    _attribute_sep: str
        The attribute separatator. Default "_"
    _kv_sep: str
        The key-value separator. Default "-"
    _file_sep: str
        The file separator. Default is platform-specific from os.path.
    _rules: list
        The rules to enforce on passed attributes

    Methods
    -------
    add_level
    add_fname
    from
    into_attributes
    """
    def __init__(
        self,
        root: str = "",
        attribute_sep: str = "_",
        kv_sep: str = "-",
        file_sep: str = None,
    ) -> None:
        """Constructs a new PathGenerator

        Parameters
        ----------
        root: str
            The root directory which all paths are relative to
        attribute_sep: str, optional
            The default delimeter for attributes. Default "_".
        kv_sep: str, optional
            The default delimiter for key-value pairs. Default "-".
        file_sep: str, optional
            The default delimeter for file paths. Default system-dependent.
            ADMONITION: unless you are writing code for another machine, it
            you should probably not override this.

        Returns
        -------
        None

        Raises
        ------
        TypeError, if anything is the wrong type
        """
        self._attributes = set()
        self._terminated = False
        self._attribute_sep = attribute_sep
        self._kv_sep = kv_sep
        if file_sep is None:
            self._file_sep = os.path.sep
        elif file_sep in ("/", "\\"):
            self._file_sep = file_sep
        else:
            raise ValueError(
                f"Specified file separator {file_sep} is not valid for most"
                " machines, terminating execution."
            )
        if root is None:
            self._components = []
        elif root == "":
            self._components = [self._file_sep]
        else:
            self._components = [root + self._file_sep]
        self._rules = []

    def add_component(
        self,
        key: str,
        delimiter: str = None,
        value_only: bool = False,
        required: bool = True,
    ) -> None:
        """Adds a name component to the path to generate

        Parameters
        ----------
        key: str
            The key that this component will use
        delimiter: str, optional
            The delimiter that this component will use. Default is to use
            the path generator's kvsep attribute. Use "" to indicate no
            delimeter. This value is ignored if value_only is True.
        value_only: bool, optional
            Whether this name component will print only the value.
        """
        self._attributes.add(key)
        if delimiter is None:
            delimiter = self._kv_sep
        nc = NameComponent(
            key, delimiter, value_only=value_only, required=required
        )
        # We have to make sure we don't have 0 elements, or else we'll have
        # an index error.
        if len(self._components) == 0:
            self._components.append(nc)
        elif isinstance(self._components[-1], NameComponent):
            # Insert the default delimiter between components
            self._components.append(self._attribute_sep)
            self._components.append(nc)
        else:
            # We already have an overridden delimiter, no need to add the
            # default delimiter
            self._components.append(nc)

    def delimiter_override(self, delimiter: str) -> None:
        """Adds a delimiter override

        Parameters
        ----------
        delimiter: str
            The delimiter to override with
        """
        self._components.append(delimiter)

    def add_filesep(self):
        """Adds a file separator to the name component list"""
        self._components.append(self._file_sep)

    def terminate(self):
        """Terminate the build pattern"""
        self._terminated = True

    # TODO: create globally required rule

    def add_inclusion_rule(
        self,
        key: str,
        value: Union[str, list],
        attributes: list
    ) -> None:
        """Make a rule that for a certain key-value domain, only some
        attributes are allowed.

        Parameters
        ----------
        key: str
            The key that is relevant to this rule
        value: str, list
            The value domain that is relevant to this rule
        attributes: list
            The list of attributes which are allowed to be used with this
            rule
        """
        # TODO: add logic to make "required" components automatically
        # allowed in all inclusion rules
        self._rules.append(InclusionRule(key, value, attributes))

    def gen_path(self, attributes: dict) -> str:
        """Generate a path with this generator and an attribute dict

        Parameters
        ----------
        attributes: dict
            The attributes to use for this path generator

        Raises
        ------
        ValueError if one of the attributes was not supplied to this path
            generator.
        """
        if not self._terminated:
            raise ValueError(
                "No path target completed!"
                "Use the terminate_path method to terminate the builder."
            )

        # subset keys in case extraneous entities are present to only
        # attributes used to build components
        subset_keys = (
            k for k in self._attributes if k in attributes.keys()
        )
        subset = {k: attributes[k] for k in subset_keys}
        for rule in self._rules:
            rule.check(subset)

        try:
            path = "".join(
                [PathGenerator._str(x, attributes) for x in self._components]
            )
        except KeyError:
            # Apparently KeyError doesn't retain which key failed
            for k in attributes.keys():
                if k not in self._attributes:
                    raise ValueError(f"Attribute {k} is not valid")

        print(path)
        path = self._strip_repeat_delimiters(path)

        return path

    def into_attributes(self, fpath: str, mode: str = "warn"):
        """Convert a path into attributes for this convention.

        Parameters
        ----------
        fpath: str
            The path to decompose into attributes.
        mode: str
            The mode to use. Default "warn."
            Each of the following is allowed for whether the program alerts
            you to a missing name requirement:
            - "loose" will not warn at all
            - "warn" will raise a python warning
            - "strict" will raise a ValueError

        Raises
        ------
        ValueError, if the filename does not contain required entities
        while the function is called with "strict" mode, or if an invalid
        mode is used.
        """
        allowed_modes = ("loose", "warn", "strict")
        if mode not in allowed_modes:
            raise ValueError(
                f"Mode {mode} is not supported; "
                f"please use one of {allowed_modes}"
            )
        atts = {}
        remaining = fpath
        reversed_nc = [x for x in reversed(self._components)]
        for curr, prev in zip(reversed_nc[0:-1:2], reversed_nc[1:-1:2]):
            split = prev
            if not curr.value_only:
                split += curr.key + curr.kv_delim
            for sp in [split]:
                parts = remaining.split(sp)
                if curr.required and len(parts) == 1:
                    if mode == "loose":
                        continue
                    elif mode == "warn":
                        warn(
                            f"Required key {curr.key} not found, "
                            f"discarded parsed part {parts[-1]}"
                        )
                    elif mode == "strict":
                        raise ValueError(f"Required key {curr.key} not found.")
                    remaining = sp.join(parts[:-1])
                elif len(parts) == 1:
                    _ = 0
                else:
                    atts[curr.key] = parts[-1]
                    remaining = sp.join(parts[:-1])
        return atts

    def to_dict(self) -> str:
        """Convert this object into a dictionary

        Returns
        -------
        Dict representation of this object.

        Notes
        -----
        Mostly for serialization into JSON but made public in case you want
        something else.
        """
        comps = []
        for c in self._components:
            if isinstance(c, str):
                if c == self._file_sep:
                    comps.append({"Key": DIRECTORY_KEYWORD})
            else:
                comps.append(c.to_dict())
        return {
            "Components": comps,
            "AttributeSeparator": self._attribute_sep,
            "KeyValueSeparator": self._kv_sep,
            "InclusionRules": [
                r.to_dict()
                for r in self._rules if isinstance(r, InclusionRule)
            ],
        }

    def to_json(self, fname: str) -> None:
        """Convert this object to a JSON file for later re-use.

        Parameters
        ----------
        fname: str
            The filename to put this object into.
        """
        print(self.to_dict())
        with open(fname, "w") as f:
            json.dump(self.to_dict(), f)

    def from_json(
            fname: str,
            root: str = "",
            file_sep: str = None,
    ):
        """Get a name generator from a JSON file

        Parameters
        ----------
        fname: str
            The JSON filename
        root: str
            The root of this name generator. Default "".
        file_sep: str
            The file separator of this name generator. Default None,
            which results in automatic choosing.

        Returns
        -------
        PathGenerator built from the file
        """
        with open(fname, "r") as f:
            this_dict = json.load(f)
        ng = PathGenerator(
            root=root,
            attribute_sep=this_dict["AttributeSeparator"],
            kv_sep=this_dict["KeyValueSeparator"],
            file_sep=file_sep
        )
        for c in this_dict["Components"]:
            if c["Key"] == DIRECTORY_KEYWORD:
                ng.add_filesep()
            else:
                ng.add_component(
                    c["Key"],
                    delimiter=c["KVDelim"],
                    value_only=c["ValueOnly"],
                    required=c["Required"]
                )

        for ir in this_dict["InclusionRules"]:
            ng.add_inclusion_rule(ir["Key"], ir["Value"], ir["Includes"])

        ng.terminate()
        return ng

    def _str(o: Union[str, "NameComponent"], att: dict) -> str:
        """Ingests a string or name component and returns a string

        Parameters
        ----------
        o: str | NameComponent
            The object to turn into a string
        att: dict
            The attribute dictionary, in case the object is a NameComponent

        Returns
        -------
        The string representation of the object
        """

        if isinstance(o, str):
            return o
        elif isinstance(o, NameComponent):
            return o.name(att)

    def _strip_repeat_delimiters(self, path: str) -> str:
        """Ingests a path and strips out repeat delimiters as defined by
        the name components in this object.

        Parameters
        ----------
        path: str
            The path to strip repeat delimiters from

        Returns
        -------
        A path with no repeat delimiters
        """
        delimiters = set()
        for c in self._components:
            if isinstance(c, str):
                delimiters.add(c)
            else:
                delimiters.add(c.kv_delim)
        for d in delimiters:
            if d != "":
                fields = path.split(d)
                path = d.join([x for x in fields if x != ""])
        return path


class NameComponent:
    """Contains information to construct name components in a path,
    constructed from attribute key-value pairs.

    Attributes
    ----------
    key: str
        The key which is used
    kv_delim: str
        The delimeter between the key and value to be used
    value_only: bool
        Whether this namer should only display the value. Default False.
    required: bool
        Whether this name component is required. Default False. If a name
        is value_only, required is automatically set to True.

    Methods
    -------
    name

    Examples
    --------
    1) Build a NameComponent with subject and ID, use defaults, print name
    >>> NameComponent("sub", "-").name({"sub": "Anthony"})
    'sub-Anthony'

    2) Build a NameComponent with only a value
    >>> NameComponent("sub", None, value_only=True).name({"sub": "01"})
    '01'

    3) Build a NameComponent that is optional, build without name
    >>> NameComponent("acq", "-", required=False).name({"sub": "01"})
    ''

    Notes
    -----
    Name components with only a value are required to be true because
    otherwise inferring the attribute from the position is required. This
    is similar in concept to the idea of positional arguments on the
    command line. At the moment we do not want to support a complex
    requirement hierarchy, and therefore enforce a binary "required" or
    "not required."
    """
    def __init__(
        self,
        key: str,
        kv_delim: str,
        value_only: bool = False,
        required: bool = True,
    ) -> None:
        self.key = key
        self.kv_delim = kv_delim
        self.value_only = value_only
        if value_only or required:
            self.required = True
        else:
            self.required = False

    def name(self, attributes: dict) -> str:
        """Names a component from the given attributes

        Parameters
        ----------
        attributes: dict
            The dictionary which contains the key-value pair to use in
            order to name the component.

        Returns
        -------
        String representing the name component.
        """
        has_key = self.key in attributes.keys()
        if self.required and not has_key:
            raise ValueError(
                f"NameComponent is required for key {self.key}, but is not"
                f" present in keys: {', '.join(attributes.keys())}"
            )
        if has_key:
            # Build the value component, which is always needed
            value = attributes[self.key]
            # Add key component if needed
            if self.value_only:
                component = value
            else:
                component = self.key + self.kv_delim + value
        else:
            return ""

        return component

    def to_dict(self) -> dict:
        return {
            "Key": self.key,
            "KVDelim": self.kv_delim,
            "ValueOnly": self.value_only,
            "Required": self.required,
        }


class InclusionRule:
    """Rule for enforcing compatibility with attributes

    Attributes
    ----------
    key: str
        The key that this rule depends on
    value: set(str)
        The value(s) that trigger the rule
    includes: set(str)
        The attributes/keys that are compatible with this key-value pair
    """
    def __init__(
        self, key: str, value: Union[str, list], includes: list
    ):
        """Constructor for InclusionRule

        Parameters
        ----------
        key: str
            The key that this rule will depend on
        value: str, list(str)
            The value(s) that trigger the rule
        includes: list(str)
            The compatible attributes for this rule
        """

        self.key = key
        if isinstance(value, str):
            value = [value]
        value = set(value)
        self.value = value
        includes = set(includes)
        self.includes = includes

    def check(self, attributes: dict):
        """Checker for the rule

        Parameters
        ----------
        attributes: dict
            The attributes to check
        """
        if not (self.key in attributes.keys()):
            return
        val = attributes[self.key]
        if val not in self.value:
            return
        # The rule applies, make sure that it's enforced
        for k in attributes.keys():
            if k not in self.includes:
                raise ValueError(
                    f"Rule violation: attribute {k} disallowed for"
                    f" key {self.key}, value {val}"
                )

    def to_dict(self) -> dict:
        return {
            "Key": self.key,
            "Value": list(self.value),
            "Includes": list(self.includes)
        }


class RequirementRule:
    pass
