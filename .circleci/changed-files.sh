#!/usr/bin/env bash
# Resolve a complete comparison; never silently lint just the last PR commit.
set -euo pipefail
if test "$(git rev-parse --is-shallow-repository)" = true; then
    git fetch --unshallow origin
fi
if test -n "${CIRCLE_PULL_REQUEST:-}"; then
    pr=${CIRCLE_PULL_REQUEST##*/}
    [[ "$pr" =~ ^[0-9]+$ ]]
    # GitHub's merge ref has the actual PR base as its first parent, including
    # PRs targeting a maintenance branch rather than master.
    git fetch origin "refs/pull/$pr/merge:refs/ci/pr-merge"
    base=$(git merge-base HEAD refs/ci/pr-merge^1)
elif test "${CIRCLE_BRANCH:-}" = master || test -n "${CIRCLE_TAG:-}"; then
    # Validate the release/master tip too; an empty diff would skip lint.
    base=$(git rev-parse HEAD^)
else
    git fetch origin master:refs/remotes/origin/master
    base=$(git merge-base HEAD refs/remotes/origin/master)
fi
python .circleci/lint.py --write-changes "$base" /tmp/afni-changes.json
