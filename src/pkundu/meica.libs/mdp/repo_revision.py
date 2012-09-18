import mdp
import os
from subprocess import Popen, PIPE, STDOUT

def get_git_revision():
    """When mdp is run from inside a git repository, this function
    returns the current revision that git-describe gives us.

    If mdp is installed (or git fails for some other reason),
    an empty string is returned.
    """
    # TODO: Introduce some fallback method that takes the info from a file
    revision = ''
    try:
        # we need to be sure that we call from the mdp dir
        mdp_dir = os.path.dirname(mdp.__file__)
        # --tags ensures that most revisions have a name even without
        # annotated tags
        # --dirty=+ appends a plus if the working copy is modified
        command = ["git", "describe", "--tags", "--dirty=+"]
        proc = Popen(command, stdout=PIPE, stderr=STDOUT, cwd=mdp_dir,
                     universal_newlines=True)
        exit_status = proc.wait()
        # only get the revision if command succeded
        if exit_status == 0:
            revision = proc.stdout.read().strip()
    except OSError:
        pass
    return revision
