from pathlib import Path


def diff_parser(dif_path):
    """Simple parser for files output by linux diff util
    """
    dif_path = Path(dif_path)
    dif_text = dif_path.read_text()
    if len(dif_text) == 0:
        return []
    dif_text = dif_text.split("\n")
    dif_res = []
    for dl in dif_text:
        dd = {}
        if len(dl) == 0:
            continue
        elif dl[0] not in "<>-":
            lb_start = int(dl.split("c")[0].split(",")[0])
            lt_start = int(dl.split("c")[1].split(",")[0])
            offset = 0
        elif dl[0] == "<":
            dd["ln"] = lb_start + offset
            dd["source"] = "baseline"
            dd["line"] = dl[2:]
            dif_res.append(dd)
            offset += 1
        elif dl[0] == ">":
            dd["ln"] = lt_start + offset
            dd["source"] = "test"
            dd["line"] = dl[2:]
            dif_res.append(dd)
            offset += 1
        elif dl[0] == "-":
            offset = 0
        else:
            raise ValueError("unexpected line start character in diff file")
    return dif_res
