from .PathGenerator import PathGenerator


def generate_bids() -> PathGenerator:
    """Generates a PathGenerator object with rules for BIDS anat, func

    Returns
    -------
    bids: PathGenerator
        The bids anatomical and functional PathGenerator
    """
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
    return bids
