from .utils import misc


def test_afni_gui():

    cmd = 'afni -no_detach -com "OPEN_WINDOW axialimage; SAVE_JPEG axialimage test1; QUIT"'

    res = misc.run_x_prog(cmd)
    assert "Fatal Signal 11" not in res
    assert "FATAL ERROR" not in res
