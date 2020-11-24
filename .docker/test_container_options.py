# Original code:
# Copyright (c) Jupyter Development Team.
# Distributed under the terms of the Modified BSD License.
import time

import pytest

PROG_LIST_TO_CHECK = (
    "afni",
    "RetroTS.py",
    "align_epi_anat.py",
    "3dinfo",
    )

def test_uid_change(container):
    """Container should change the UID of the default user."""
    c = container.run(
        tty=True,
        user='root',
        environment=['CONTAINER_UID=1010'],
        command=['start.sh', 'bash', '-c', 'id && touch /home/afni_user/test-file']
    )
    # usermod is slow so give it some time
    c.wait(timeout=120)
    stdout = c.logs(stdout=True).decode('utf-8')
    if 'uid=1010(afni_user)' not in stdout:
        print(stdout)
        raise EnvironmentError(
            "User id is not being changed correctly"
        )



def test_gid_change(container):
    """Container should change the GID of the default user."""
    c = container.run(
        tty=True,
        user='root',
        environment=['CONTAINER_GID=110'],
        command=['start.sh', 'id']
    )
    c.wait(timeout=10)
    logs = c.logs(stdout=True).decode('utf-8')
    assert 'gid=110(afni_user)' in logs
    assert 'groups=110(afni_user),100(users)' in logs


def test_chown_extra(container):
    """Container should change the UID/GID of CHOWN_EXTRA."""
    c = container.run(
        tty=True,
        user='root',
        environment=['CONTAINER_UID=1010',
                     'CONTAINER_GID=101',
                     'CHOWN_EXTRA=/usr/bin',
                     'CHOWN_EXTRA_OPTS=-R',
        ],
        command=['start.sh', 'bash', '-c', 'stat -c \'%n:%u:%g\' /usr/bin/python']
    )
    # chown is slow so give it some time
    c.wait(timeout=5)
    if not '/usr/bin/python:1010:101' in c.logs(stdout=True).decode('utf-8'):
        print(c.logs(stdout=True).decode('utf-8'))
        raise EnvironmentError(
            "Expected to find an executable python in the container"
        )


def test_chown_home(container):
    """Container should change the CONTAINER_USER home directory owner and 
    group to the current value of CONTAINER_UID and CONTAINER_GID."""
    c = container.run(
        tty=True,
        user='root',
        environment=['CONTAINER_UID=1010',
                     'CONTAINER_GID=101',
                     'CHOWN_HOME=yes',
                     'CHOWN_HOME_OPTS=-R',
        ],
        command=['start.sh', 'bash', '-c', 'stat -c \'%n:%u:%g\' /home/afni_user']
    )
    c.wait(timeout=2)
    assert "Changing ownership of /home/afni_user to 1010:101 with options '-R'" in c.logs(stdout=True).decode('utf-8')


def test_sudo(container):
    """Container should grant passwordless sudo to the default user."""
    c = container.run(
        tty=True,
        user='root',
        environment=['GRANT_SUDO=yes'],
        command=['start.sh', 'sudo', 'id']
    )
    rv = c.wait(timeout=10)
    assert rv == 0 or rv["StatusCode"] == 0
    assert 'uid=0(root)' in c.logs(stdout=True).decode('utf-8')


def test_sudo_path(container):
    """Container should have usable /usr/local/bin in the sudo secure_path."""
    c = container.run(
        tty=True,
        user='root',
        environment=['GRANT_SUDO=yes'],
        command=['start.sh', 'sudo', 'which', 'apt-get']
    )
    rv = c.wait(timeout=10)
    assert rv == 0 or rv["StatusCode"] == 0
    assert c.logs(stdout=True).decode('utf-8').rstrip().endswith('/usr/local/bin/apt-get')


def test_sudo_path_without_grant(container):
    """Container should have usable /usr/local/bin in the unprivileged user"""
    c = container.run(
        tty=True,
        user='root',
        command=['start.sh', 'which', 'apt-get']
    )
    rv = c.wait(timeout=10)
    assert rv == 0 or rv["StatusCode"] == 0
    assert c.logs(stdout=True).decode('utf-8').rstrip().endswith('/usr/local/bin/apt-get')


def test_group_add(container, tmpdir):
    """Container should run with the specified uid, gid, and secondary
    group.
    """
    c = container.run(
        user='1010:1010',
        group_add=['users'],
        command=['start.sh', 'id']
    )
    rv = c.wait(timeout=5)
    assert rv == 0 or rv["StatusCode"] == 0
    expected = 'uid=1010 gid=1010 groups=1010,100(users)'
    assert expected in c.logs(stdout=True).decode('utf-8')

@pytest.mark.parametrize(
    "env_tweaks",
    (
        ['GRANT_SUDO=yes'],
        ['CONTAINER_UID=1010'],
        ['CONTAINER_UID=1010','CONTAINER_GID=120','CHOWN_EXTRA=/opt','CHOWN_EXTRA_OPTS=-R'],
    )
)
@pytest.mark.parametrize(
    "named_container",
    (
        "afni/afni_make_build",
        "afni/afni_cmake_build",
    ),
    indirect=True,
)
def test_various_programs_are_found(named_container,env_tweaks):
    """Working containers should be able to find the installed binaries, scripts,etc"""

    c = named_container.run(
        tty=True,
        user='root',
        environment=[*env_tweaks],
        detach=True
    )
    # rv = c.wait(timeout=120)
    # assert rv == 0 or rv["StatusCode"] == 0
    # Check that each program is available (on the PATH and executable)
    for prog in PROG_LIST_TO_CHECK:
        res = c.exec_run(["which", prog])
        if not res.output.decode('utf-8').rstrip().endswith(prog):
            print(res.output.decode('utf-8').rstrip())
            raise EnvironmentError(f"Could not find the '{prog}' executable in {named_container.image_name}")



@pytest.mark.parametrize( "named_container", ( "afni/afni_make_build",
                                              "afni/afni_cmake_build",),
                         indirect=True,)
def test_singularity_like_restrictions(named_container):
    """Working containers should be able to find the installed binaries,
    scripts,etc even with restricted access"""
    c = named_container.run(
        tty=True,
        detach=True,
        read_only=True,
        tmpfs = {'/run':'', '/tmp':''},
    )
    # Check that each program is available (on the PATH and executable)
    for prog in PROG_LIST_TO_CHECK:
        res = c.exec_run(["which", prog])
        assert res.output.decode('utf-8').rstrip().endswith(prog)


