#!/usr/bin/env bash
# Build once into a builder image so caches/reports can be extracted without
# publishing them. The final multi-stage image reuses those exact build layers.
set -euo pipefail
build_type=$1
mkdir -p .ci-cache/ccache /tmp/build-reports
cp /tmp/afni-changes.json .ci-cache/changes.json
builder="afni/afni_${build_type}:builder"
args=(-f ".docker/$build_type.dockerfile")
if test "$build_type" = cmake_build; then
    args+=(--build-arg "BUILD_JOBS=${BUILD_JOBS:-2}")
fi
docker build "${args[@]}" --target builder -t "$builder" .
if test "$build_type" = cmake_build; then
    container=$(docker create "$builder" python /opt/afni/src/.circleci/check-build.py cmake \
        --source /opt/afni/src --build /opt/afni/build --reports /opt/build-reports)
else
    container=$(docker create "$builder" true)
fi
trap 'docker rm -f "$container" >/dev/null' EXIT
set +e
docker start --attach "$container"
check_status=$(docker inspect --format '{{.State.ExitCode}}' "$container")
set -e
docker cp "$container:/opt/build-reports/." /tmp/build-reports/
test "$check_status" = 0
# Build the runtime stage before exporting ccache: changing the seed inside
# the source context would invalidate COPY and rebuild the compiler stage.
docker build "${args[@]}" -t "afni/afni_$build_type" .
docker cp "$container:/opt/ccache/." .ci-cache/ccache/
docker run --rm "afni/afni_$build_type" python /opt/afni/src/.circleci/smoke.py "$build_type"
