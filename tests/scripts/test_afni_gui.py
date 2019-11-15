from .utils import misc


def test_afni_gui():

    cmd = 'afni -no_detach -com "OPEN_WINDOW axialimage; SAVE_JPEG axialimage test1; QUIT"'

    res = misc.run_x_prog(cmd)
    assert "Fatal Signal 11" not in res
    assert "FATAL ERROR" not in res


def test_suma_gui():

    cmd = "suma"

    res = misc.run_x_prog(cmd)
    assert "Fatal Signal 11" not in res
    assert "FATAL ERROR" not in res


def test_more_extensive_gui_testing():

    cmd = "@DO.examples -auto_test"

    res = misc.run_x_prog(cmd)
    assert "Fatal Signal 11" not in res
    assert "FATAL ERROR" not in res
