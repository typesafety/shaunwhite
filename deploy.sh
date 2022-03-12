#!/bin/bash

set -e

# Print and check that all environment variables are set
for env_var in REMOTE REMOTE_WORKDIR
do
    echo $env_var: ${!env_var}
    if [ -z ${!env_var} ]; then
        echo "Variable $env_var not set"
        exit 1
    fi
done

set -x

export DOCKER_BUILDKIT=1

# Build and export the built image
docker build -t shaunwhite-release .
docker image save shaunwhite-release:latest -o shaunwhite-release.tar

# Transfer the exported image
scp shaunwhite-release.tar docker-compose.yml "${REMOTE}:${REMOTE_WORKDIR}"

# Deploy
ssh "${REMOTE}" \
    "cd ${REMOTE_WORKDIR}; \
    docker load --input shaunwhite-release.tar; \
    docker compose up --force-recreate --detach shaunwhite;"
