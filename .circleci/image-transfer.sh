#!/usr/bin/env bash
# Optional transport for trusted pipelines. Pipeline-scoped tags survive reruns;
# release tags/latest are only written by the gated deploy job.
set -euo pipefail
action=$1
build_type=$2
: "${AFNI_CI_REGISTRY:?Set AFNI_CI_REGISTRY to a registry/repository prefix}"
: "${AFNI_CI_PIPELINE_ID:?}"
: "${CIRCLE_SHA1:?}"
image="${AFNI_CI_REGISTRY}/afni_${build_type}:${CIRCLE_SHA1}-${AFNI_CI_PIPELINE_ID}"
if test "$action" = push; then
    : "${AFNI_CI_REGISTRY_USER:?}"
    : "${AFNI_CI_REGISTRY_PASSWORD:?}"
    echo "$AFNI_CI_REGISTRY_PASSWORD" | docker login "${AFNI_CI_REGISTRY%%/*}" -u "$AFNI_CI_REGISTRY_USER" --password-stdin
    docker tag "afni/afni_${build_type}" "$image"
    docker push "$image"
elif test "$action" = pull; then
    if test -n "${AFNI_CI_REGISTRY_USER:-}"; then
        echo "$AFNI_CI_REGISTRY_PASSWORD" | docker login "${AFNI_CI_REGISTRY%%/*}" -u "$AFNI_CI_REGISTRY_USER" --password-stdin
    fi
    docker pull "$image"
    docker tag "$image" "afni/afni_${build_type}"
else
    echo "Expected push or pull" >&2
    exit 2
fi
