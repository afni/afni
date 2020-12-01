import sys


def setup_exceptionhook():
    """
    Overloads default sys.excepthook with our exceptionhook handler.
    If interactive, our exceptionhook handler will invoke pdb.post_mortem;
    if not interactive, then invokes default handler.
    """

    def _pdb_excepthook(type, value, tb):
        import traceback
        import pdb

        traceback.print_exception(type, value, tb)
        # print()
        pdb.post_mortem(tb)

    sys.excepthook = _pdb_excepthook
