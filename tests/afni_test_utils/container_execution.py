from pathlib import Path
import os
import subprocess as sp
import docker

from afni_test_utils.minimal_funcs_for_run_tests_cli import (
    VALID_MOUNT_MODES,
    check_build_directory,
    )


def run_containerized(tests_dir, **kwargs):
    """
    Runs the afni tests in a container. Kwargs are populated by the result
    of parsing user arguments from the commandline in run_afni_tests.py
    """

    # Do a basic check of user args
    check_user_container_args(tests_dir, **kwargs)

    # Set a default image if not provided
    if not kwargs.get("image_name"):
        kwargs["image_name"] = "afni/afni_cmake_build"
    image_name = kwargs["image_name"]

    client = docker.from_env()

    if not client.images.list(image_name):
        if kwargs.get("only_use_local"):
            raise ValueError(f"Cannot find the image {image_name}")
        else:
            print(f"Trying to download {image_name}. This will take a while...")
            client.images.pull(f"{image_name}:latest")

    # Manage container id and mounted volumes:
    docker_kwargs = setup_docker_env_and_vol_settings(tests_dir, **kwargs)

    if kwargs.get("debug"):
        raise NotImplementedError(
            "Consider running the container from the command line and "
            "then executing the script in debug mode from within the "
            "container. "
        )

        #  The following does not work. debugpy may be a way of attaching to
        #  the container's python process in a way that facilitates pdb usage.
        #  May work through this at some point.
        print(
            "Streamed stdout is not supported with debugging. No "
            "output will be observed until an error triggers pdb "
            "entry or the tests finish. "
        )

        docker_kwargs["detach"] = False
        # docker_kwargs["auto_remove"] = True
        docker_kwargs["stdin_open"] = True
        docker_kwargs["tty"] = True
    else:
        docker_kwargs["detach"] = True

    # Convert parsed user args for execution in the container
    converted_args = unparse_args_for_container(**kwargs)

    # Run the test script inside the container
    script_path = "/opt/afni/src/tests/run_afni_tests.py"
    print(f"Starting to run the docker container (from the image {image_name})...")

    # cmd = f"""/usr/bin/python -c 'import pdb;pdb.set_trace()'"""
    # cmd = f"""/bin/sh -c 'echo hello;sleep 5;echo bye bye'"""
    cmd = f"""/bin/sh -c '{script_path} {converted_args}'"""
    output = client.containers.run(image_name, cmd, **docker_kwargs)
    if not kwargs.get("debug"):
        for line in output.logs(stream=True):
            print(line.decode("utf-8"))
    else:
        print(output.decode("utf-8"))


def add_coverage_env_vars(docker_kwargs, **kwargs):
    if kwargs.get("coverage"):
        res = sp.check_output(
            "/bin/bash -c 'bash <(curl -s https://codecov.io/env)'", shell=True
        )
        ci_vars = [x for x in res.decode().split("-e") if x]
        try:
            ci_dict = {x: os.environ[x] for x in ci_vars}
            docker_kwargs["environment"].update(ci_dict)
        except KeyError:
            print(
                "It appears you are not in a CI environment. Will not "
                "attempt to upload coverage "
            )
    return docker_kwargs


def get_path_strs_for_mounting(tests_dir):
    data_relpath = "tests/afni_ci_test_data"
    host_src = str(tests_dir.parent)
    host_data = str(Path(host_src) / data_relpath)
    container_src = "/opt/afni/src"
    container_data = str(Path(container_src) / data_relpath)
    return host_src, host_data, container_src, container_data


def setup_docker_env_and_vol_settings(tests_dir, **kwargs):
    docker_kwargs = {"environment": {}, "volumes": {}}

    # Setup ci variables outside container if required
    docker_kwargs = add_coverage_env_vars(docker_kwargs, **kwargs)

    # Define some paths
    hsrc, hdata, csrc, cdata = get_path_strs_for_mounting(tests_dir)
    ctest = str(Path(csrc) / "tests")

    # Mount build directory if provided (needs to be container perms if not
    # combined with mounting the source):
    if kwargs.get("build_dir"):
        bdir = "/opt/afni/build"
        docker_kwargs["volumes"][kwargs["build_dir"]] = {"bind": bdir, "mode": "rw"}

    # Set the container user to root so that chowning files and changing
    # container id is allowed. Note the tests are run as CONTAINER_UID
    if os.getuid != "0":
        docker_kwargs["user"] = "root"

    if "test-data-only" == kwargs.get("source_mode"):
        docker_kwargs["volumes"][hdata] = {"bind": cdata, "mode": "rw"}
        docker_kwargs["environment"].update(
            {"CHOWN_EXTRA": f"{cdata}", "CHOWN_EXTRA_OPTS": "-R"}
        )
    elif kwargs.get("source_mode") == "test-code":
        # Chowning everything to the host id is the most robust approach when
        # mounting the source
        docker_kwargs["volumes"][str(tests_dir)] = {"bind": ctest, "mode": "rw"}
        docker_kwargs["environment"].update(
            {
                "CHOWN_HOME": "yes",
                "CHOWN_HOME_OPTS": "-R",
                "CHOWN_EXTRA": "/opt/afni/install,/opt/user_pip_packages",
                "CHOWN_EXTRA_OPTS": "-R",
                "CONTAINER_UID": os.getuid(),
                "CONTAINER_GID": os.getgid(),
            }
        )
    elif kwargs.get("source_mode") == "host":
        # Chowning everything to the host id is the most robust approach when
        # mounting the source.
        docker_kwargs["volumes"][hsrc] = {"bind": csrc, "mode": "rw"}
        dirs_to_change = "/opt/afni/build,/opt/user_pip_packages"
        docker_kwargs["environment"].update(
            {
                "CHOWN_HOME": "yes",
                "CHOWN_HOME_OPTS": "-R",
                "CHOWN_EXTRA": dirs_to_change,
                "CHOWN_EXTRA_OPTS": "-R",
                "CONTAINER_UID": os.getuid(),
                "CONTAINER_GID": os.getgid(),
            }
        )
    # else:
    #     raise NotImplementedError

    docker_kwargs["environment"].update({"PYTHONUNBUFFERED": "0"})

    return docker_kwargs


def check_user_container_args(tests_dir, **kwargs):
    """
    Run some checks that are quick and easy before interacting with docker.
    """

    # Cannot mount a build dir and reuse container build or mount test-code
    # The former implies that the container's cmake build is used for testing
    # The latter implies that the installed afni in the container is used.

    source_mode = kwargs.get("source_mode")
    build_dir = kwargs.get("build_dir")
    # Must use a valid source-mode
    if source_mode and source_mode not in VALID_MOUNT_MODES:
        raise ValueError(f"Valid options for source-mode are {VALID_MOUNT_MODES}")

    cmake_build_used = kwargs.get("reuse_build") or build_dir
    if source_mode == "test-code" and cmake_build_used:
        raise ValueError(
            "mounting 'test-code' implies you are using the installed "
            "version of afni so you will not be using the build "
            "directory. Accordingly you should not use --build-dir or "
            "--reuse-build. "
        )

    if source_mode == "host" and not cmake_build_used:
        raise ValueError(
            "--source-mode=host implies you want to rebuild using "
            "cmake. You have not passed --reuse-build or --build-dir. "
        )

    if build_dir and kwargs.get("reuse_build"):
        raise ValueError(
            "--reuse-build uses the build directory in the container, "
            "you cannot mount into that directory with --build-dir "
        )

    # raise error if build is within source and both are mounted
    if source_mode == "host" and build_dir:
        assert Path(build_dir).is_absolute()

        host_src, *_ = get_path_strs_for_mounting(tests_dir)
        try:
            # If this raises an error then the build is outside the source
            # which is what we want
            Path(build_dir).relative_to(host_src)
        except ValueError:
            # build dir not in source dir
            pass
        else:
            raise ValueError(
                "The build directory is within the source directory. "
                "This has issues when the source directory is being "
                "mounted as you are trying to mount the build directory "
                "in two locations in the container... "
            )

    # build dir should have been from a previous build in the container
    check_build_directory(build_dir, within_container=True)

    user_id = os.getuid()
    if user_id == "0":

        # root user, only mount-mode of 'none' or 'test-data' are valid.
        # Overall, the testing should be run as a non-root user. The only time
        # where this might reasonably be expected is in docker-git-ce git
        # container used for coverage testing on circleci
        if kwargs.get("source_mode") != "test-data-only":
            raise ValueError(
                "You are executing tests as a root user. You cannot "
                "mount the source directory from the host. "
            )

        if "build_dir" in kwargs:
            raise ValueError(
                "You are executing tests as a root user. You cannot "
                "mount a build directory into the container. "
            )


def unparse_args_for_container(**kwargs):
    """
    Reconstructs the arguments passed to run_afni_tests.py. This is along the
    lines of unparsing the arguments but:
    a) it also removes any arguments that were relevant and used for modifying
       the behavior of the container execution (volume mounting etc)
    b) the output is a string rather than the list from sys.argv
    """
    cmd = ""
    for k, v in kwargs.items():
        if k in ["source_mode", "image_name", "only_use_local", "subparser"]:
            pass
        elif v in [None, False]:
            pass
        elif k in ["build_dir", "reuse_build"]:
            cmd += " --build-dir=/opt/afni/build"
        elif k == "extra_args":
            cmd += f' --extra-args="{v}"'
        elif k == "filter_expr":
            cmd += f' -k="{v}"'
        elif k == "verbose":
            cmd += f" -{v * 'v'}"
        elif v is True:
            cmd += f" --{k.replace('_','-')}"
        else:
            raise NotImplementedError(
                f"Behavior for passing {k,v} to container is undefined"
                )
    cmd += " local"
    return cmd


