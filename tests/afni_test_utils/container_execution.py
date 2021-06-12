from pathlib import Path
import os
import sys
import subprocess as sp
import inspect
import docker
import logging
import requests

from afni_test_utils.minimal_funcs_for_run_tests_cli import (
    VALID_MOUNT_MODES,
    check_if_cmake_configure_required,
    check_git_config,
)


def get_docker_image(
    client, image_name, only_use_local, search_intermediate_layers=False
):

    images_found = client.images.list(image_name)

    # do a more extensive local search to see if a id hash was given
    if not images_found and (search_intermediate_layers and "/" not in image_name):
        for image in client.images.list(all=True):
            if image.id.replace("sha256:", "").startswith(image_name):
                images_found.append(image)
    # Check if the image can be pulled from dockerhub
    if not images_found:
        if only_use_local:
            raise ValueError(f"Cannot find the image {image_name}")
        else:
            print(
                f"Cannot find {image_name} locally. Will try to "
                "download, this will take a while... "
            )

            if ":" not in image_name:
                # Pull latest not all the images!
                print(
                    f"An image version for {image_name} was not "
                    "specified, using 'latest' "
                )

                image_name += ":latest"
            client.images.pull(f"{image_name}")
            images_found = client.images.list(image_name)

    if len(images_found) > 1:
        print(f"More than one image has been found for '{image_name}'.")
        for image in images_found:
            if any(x.endswith(":latest") for x in image.tags):
                print(f"Using image with the tags: {image.tags}")
                return image
        else:
            raise ValueError(
                f"Looking for image name: {image_name}. There is "
                "ambiguity in this as there are more than one image by "
                f"that name: {images_found}. Try using a more specific image_name."
            )

    else:
        image = images_found[0]
    return image


def setup_test_data_vol(client, kwargs, docker_kwargs, host_data, container_data):
    """Create a container called test-data with a volume
    containing the test data. User/data ownership should match
    the testing container so that the volume will be usable.

    This is used for making the test data available to the remote docker
    container on circleci. Outside of this usecase it probably not needed and
    should not be used as it is needlessly complicated (just mount the source
    repository into the container using --source-mode=host)

    Args:
        client (DockerClient): created using docker.from_env()
        kwargs (dict): Arguments provided by user

        docker_kwargs (dict): Provided to the testing container to configure
        it for the tests execution. Includes volume mounting, user id,
        permissions etc.
    """
    # remove pre-existing test-data container if it exists
    for container in client.containers.list(all=True):
        if container.name == "test_data":
            container.stop()
            container.remove()

    # initial setup in the container
    # The default is ownership is 1000:100 (as defined in afni_dev_base.dockerfile)
    uid = docker_kwargs.get("CONTAINER_UID") or 1000
    gid = docker_kwargs.get("CONTAINER_GID") or 100
    setup_cmd = [
        "/bin/bash",
        "-c",
        (
            f"useradd -u {uid} -g {gid} -lMN data_user;"
            f"chown -R {uid}:{gid} /opt;"
            "echo starting test_data container;"
            "sleep 1m"
        ),
    ]
    host_config = client.api.create_host_config(
        binds=[
            f"{host_data}/.:{container_data}",
        ]
    )
    logging.info(
        "Setting up container for the test data volume using the "
        "afni/afni_circleci_executor image. This will take some time. "
    )
    ci_image = "afni/afni_circleci_executor"

    get_docker_image(client, ci_image, only_use_local=False)
    container = client.api.create_container(
        ci_image,
        setup_cmd,
        name="test_data",
        volumes=[container_data],
        host_config=host_config,
    )
    client.api.start("test_data")
    # exec_id = client.api.exec_create('test_data','echo hello'.split())['Id']
    # client.api.exec_start(exec_id)
    return container

    # data_setup_args = 'echo hello'.split()
    # create the container
    container = client.api.create_container(
        "afni/afni_circleci_executor",
        "sleep 1m",
        user="root",
        name="test_data",
        detach=True,
        volumes=[container_data],
        host_config=host_config,
    )
    client.api.start(container["Id"])
    exec_id = client.api.exec_create(container["Id"], "echo hello")["Id"]
    client.api.exec_start(exec_id)
    return container


def run_containerized(tests_dir, **kwargs):
    """
    Runs the afni tests in a container. Kwargs are populated by the result
    of parsing user arguments from the commandline in run_afni_tests.py
    """
    # Set a default image if not provided
    if not kwargs.get("image_name"):
        kwargs["image_name"] = "afni/afni_cmake_build"
    image_name = kwargs["image_name"]

    # Do a basic check of user args
    check_user_container_args(tests_dir, **kwargs)

    client = docker.from_env()

    docker_py_error = (
        "It appears that you have pip installed docker-py. You should "
        "either pip install docker, or conda install docker-py. "
        "Confusing to be sure... docker-py via pip is a similar but "
        "different package. "
    )

    if inspect.ismethod(client.images):
        print(docker_py_error)
        sys.exit(1)
    image = get_docker_image(
        client, image_name, kwargs.get("only_use_local"), kwargs.get("intermediate")
    )

    # Manage container id and mounted volumes:
    docker_kwargs = setup_docker_env_and_volumes(client, tests_dir, **kwargs)
    if kwargs.get("debug"):
        raise_error_for_debug_mode(tests_dir, kwargs, docker_kwargs)
        #  The following does not work. debugpy may be a way of attaching to
        #  the container's python process in a way that facilitates pdb usage.
        #  May work through this at some point.
        # docker_kwargs["detach"] = False
        # # docker_kwargs["auto_remove"] = True
        # docker_kwargs["stdin_open"] = True
        # docker_kwargs["tty"] = True
    else:
        docker_kwargs["detach"] = True

    if kwargs.get("container_name"):
        docker_kwargs["name"] = kwargs.pop("container_name")

    # Convert parsed user args for execution in the container
    converted_args = unparse_args_for_container(tests_dir, **kwargs)

    # Run the test script inside the container
    script_path = "/opt/afni/src/tests/run_afni_tests.py"
    print(f"Starting to run the docker container (from the image {image_name})...")

    # cmd = f"""/usr/bin/python -c 'import pdb;pdb.set_trace()'"""
    # cmd = f"""/bin/sh -c 'echo hello;sleep 5;echo bye bye'"""
    cmd = f"""/bin/sh -c '{script_path} {converted_args}'"""
    try:
        try:
            output = client.containers.run(image, cmd, **docker_kwargs)
        except requests.exceptions.ReadTimeout as err:
            print(err)
            sys.exit(
                "ERROR: Not sure why this times out on occasion. Removing some docker processes can sometimes help, or just rerun the command."
            )

        if True:
            for line in output.logs(stream=True):
                print(line.decode("utf-8"))
        else:
            # might be required if detach is not set to True
            print(output.decode("utf-8"))

        result = output.wait()
        # Propagate container exit error code
        if result["StatusCode"]:
            raise SystemExit(result["StatusCode"])
    finally:
        # Remove exited container
        if not kwargs.get("no_rm"):
            if "output" in locals():
                output.remove(force=True)


def add_coverage_env_vars(docker_kwargs, **kwargs):
    if kwargs.get("coverage"):
        res = sp.check_output(
            "/bin/bash -c 'bash <(curl -s https://codecov.io/env)'", shell=True
        )
        ci_vars = [x.strip() for x in res.decode().split("-e") if x]
        ci_dict = {x: os.environ.get(x) for x in ci_vars}
        docker_kwargs["environment"].update(ci_dict)
    return docker_kwargs


def get_path_strs_for_mounting(tests_dir):
    data_relpath = "tests/afni_ci_test_data"
    host_src = str(tests_dir.parent)
    host_data = str(Path(host_src) / data_relpath)
    container_src = "/opt/afni/src"
    container_data = str(Path(container_src) / data_relpath)
    return host_src, host_data, container_src, container_data


def user_is_root():
    return os.getuid() == 0


def add_git_credential_env_vars(docker_kwargs, **kwargs):
    if not kwargs.get("do_not_forward_git_credentials"):
        name, email = check_git_config()

        docker_kwargs["environment"].update(
            {"GIT_AUTHOR_NAME": name, "GIT_AUTHOR_EMAIL": email}
        )
    return docker_kwargs


def setup_docker_env_and_volumes(client, tests_dir, **kwargs):
    docker_kwargs = {"environment": {}, "volumes": {}}

    # Setup ci variables outside container if required
    docker_kwargs = add_coverage_env_vars(docker_kwargs, **kwargs)
    # Pass git credentials into container
    docker_kwargs = add_git_credential_env_vars(docker_kwargs, **kwargs)

    # Define some paths
    hsrc, hdata, csrc, cdata = get_path_strs_for_mounting(tests_dir)
    ctest = str(Path(csrc) / "tests")

    # Mount build directory if provided (needs to be user perms)
    if kwargs.get("build_dir"):
        bdir = "/opt/afni/build"
        docker_kwargs["volumes"][kwargs["build_dir"]] = {"bind": bdir, "mode": "rw"}

    # Set the container user to root so that chowning files and changing
    # container id is allowed. Note the tests are run as CONTAINER_UID
    docker_kwargs["user"] = "root"

    if kwargs.get("source_mode") == "test-data-volume":
        # Mount volume from running container "test_data", used for circleci.
        # This option is somewhat silly and is contorted usage to satisfy some
        # of the implementation details of docker execution on circleci. Stay
        # away!
        docker_kwargs["volumes_from"] = ["test_data"]

        if not user_is_root():
            # This is poorly supported. Main reason to use test-data-volume
            # mount mode is for circleci (user in container is root), otherwise just use the 'host' source
            # mode.
            docker_kwargs["environment"].update(
                {
                    "CHOWN_HOME": "yes",
                    "CHOWN_HOME_OPTS": "-R",
                    "CHOWN_EXTRA_OPTS": "-R",
                }
            )
            docker_kwargs["environment"].update(
                {
                    "CHOWN_EXTRA": "/opt/afni/install,/opt/user_pip_packages,/opt/afni/build",
                    "CONTAINER_UID": os.getuid(),
                    "CONTAINER_GID": os.getgid(),
                }
            )

        data_container = setup_test_data_vol(
            client, kwargs, docker_kwargs, hdata, cdata
        )

    elif kwargs.get("source_mode") == "test-code" and kwargs.get("reuse_build"):
        # test code being used in conjunction with the cmake build
        # need to chown build, all source except test, and pip/home dirs
        raise NotImplementedError()
    elif kwargs.get("source_mode") == "test-code":
        # test code being used in conjunction with code installed in the
        # container. --abin may or may not have been used (for the cmake
        # installation, and it probably won't work)
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
        dirs_to_change = "/opt/user_pip_packages"
        if not kwargs.get("build_dir"):
            dirs_to_change += ",/opt/afni/build"
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
            "--source-mode=host implies you want to build using cmake. "
            "You have not passed --reuse-build (as an option to the "
            "container sub command) or --build-dir (as a general option). "
        )

    if build_dir and kwargs.get("reuse_build"):
        raise ValueError(
            "--reuse-build uses the build directory in the container, "
            "you cannot mount into that directory with --build-dir "
        )

    if build_dir and not source_mode == "host":
        raise ValueError(
            "You have specified an unsupported combination of options. "
            "You specified a build directory without specifying a "
            "source-mode of 'host'. This could technically be performed "
            "but the downside is that the uid inside the container "
            "would be used and all permissions/ownership for the build "
            "directory would be for said user. This would then cause "
            "issues for when you do wish to use this build directory in "
            "the container when using source-mode of host, which uses "
            "your local uid/gid. The solution... not to support what "
            "you are trying to do. It is unlikely to be especially "
            "useful. Most likely, when using containers, you either "
            "want to use a source- mode=host in combination with "
            "--build-dir or an alternative source-mode without "
            "specifying --build-dir. "
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

    if kwargs["image_name"] == "afni/afni_make_build" and kwargs.get("coverage"):
        raise ValueError(
            "Performing coverage testing with the make build is not supported."
        )

    # build dir should have been from a previous build in the container
    check_if_cmake_configure_required(build_dir, within_container=True)

    if user_is_root():

        # root user, only mount-mode test-data mounting is supported. Overall,
        # the testing should be run as a non-root user. The only time where
        # this might reasonably be expected is in docker-git-ce container used
        # for coverage testing on circleci
        if kwargs.get("source_mode") and "test-data" not in kwargs.get("source_mode"):
            raise ValueError(
                "You are executing tests as a root user. You cannot "
                "mount the source directory from the host."
            )

        if "build_dir" in kwargs:
            raise ValueError(
                "You are executing tests as a root user. You cannot "
                "mount a build directory into the container. "
            )


def raise_error_for_debug_mode(tests_dir, kwargs, docker_kwargs):
    debug_not_supported = r"""\
    ERROR:

    It appears you are trying to use the debug flag with a container. This is
    not directly supported (stdout/stderr/stdin capture gets confusing fast).
    You could use -vvvvvv to see what is happening in the container. Also
    note, to debug tests that are only failing in  container you can mount
    your local source code into the container and then continue on as you
    usually would (with the debug flag, the build will be in /opt/afni/build,
    and the source will be in /opt/afni/src) but to use the "local" subparser
    (since your terminal is now providing you a shell in the container). An
    example command is displayed below.



    Consider copying and pasting the following and executing in bash to access
    a container with the appropriate user setup, file permissions, and
    source-code from your local host:

    \
    mkdir -p /tmp/container_build;                                                  \
    docker run                                                                      \
        `# root needed for changing file permissions`                               \
        --user=root                                                                 \
        `# Issues can happen when write access to the home directory is missing`    \
        -e CHOWN_HOME="yes"                                                         \
        -e CHOWN_HOME_OPTS='-R'                                                     \
        `# This allows package installation via pip`                                \
        -e CHOWN_EXTRA="/opt/user_pip_packages,"                                    \
        -e CHOWN_EXTRA_OPTS='-R'                                                    \
        `# Allow sudo execution once in the container`                              \
        -e GRANT_SUDO=yes                                                           \
        `# The id of the local host is used inside the container. This minimizes`   \
        `# issues with file permissions`                                            \
        -e CONTAINER_UID=$(id -u)                                                   \
        -e CONTAINER_GID=$(id -g)                                                   \
        `# Pass git credentials into container for more pleasant interaction `      \
        `# with the source git repository`                                          \
        -e GIT_AUTHOR_NAME={docker_kwargs['environment']['GIT_AUTHOR_NAME']}        \
        -e GIT_AUTHOR_EMAIL={docker_kwargs['environment']['GIT_AUTHOR_EMAIL']}      \
        `# Delete the container upon exit`                                          \
        --rm                                                                        \
        `# provide an interactive terminal`                                         \
        -ti                                                                         \
        `# this works optimally  (local codebase mounted inside the container)`     \
        -v {tests_dir.parent.absolute()}:/opt/afni/src                              \
        `# Mounting this directory  helps for permissions and build reuse`          \
        -v /tmp/container_build:/opt/afni/build                                     \
        `# using the container with the cmake build is advised...`                  \
        afni/afni_cmake_build
    """
    if "debug" in kwargs:
        # The following is hokey but prevent some weird editor error (it doesn't
        # like the combination of format strings and raw strings)
        debug_not_supported = eval(f'fr"""{debug_not_supported}"""')
        # Advise on debugging in a container...
        print(debug_not_supported)
        raise SystemExit(1)


def unparse_args_for_container(tests_dir, **kwargs):
    """
    Reconstructs the arguments passed to run_afni_tests.py. This is along the
    lines of unparsing the arguments but it also removes any arguments that
    were relevant and used for modifying the behavior of the container
    execution (volume mounting etc).


    """
    cmd = ""
    for k, v in kwargs.items():
        if k in [
            "source_mode",
            "image_name",
            "only_use_local",
            "subparser",
            "do_not_forward_git_credentials",
            "no_rm",
        ]:
            pass
        elif v in [None, False]:
            pass
        elif k in ["build_dir", "reuse_build"]:
            cmd += " --build-dir=/opt/afni/build"
        elif k == "extra_args":
            cmd += f' --extra-args="{v}"'
        elif k == "filter_expr":
            cmd += f' -k="{v}"'
        elif k == "marker_expression":
            cmd += f" -m='{v}'"
        elif k == "verbosity":
            cmd += f" -v {v}"
        elif k == "file":
            cmd += f" --file=/opt/afni/src/tests/{Path(v).relative_to(tests_dir)}"
        elif v is True:
            cmd += f" --{k.replace('_','-')}"
        else:
            raise NotImplementedError(
                f"Behavior for passing {k,v} to container is undefined"
            )
    cmd += " local"
    return cmd
