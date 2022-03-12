#
# Build the thing
#
FROM haskell:8.10.7 as build-env

WORKDIR /opt/shaunwhite

RUN cabal update

# Build dependencies so that they are cached
COPY shaunwhite.cabal cabal.project.freeze .

# Build dependencies only so that they are cached
RUN cabal build --only-dependencies

COPY main ./main
COPY src ./src
COPY test ./test
COPY LICENSE \
    README.md \
    CHANGELOG.md \
    Makefile \
    .

# Build application
RUN cabal install --installdir=.

#
# Copy to smaller image
#
FROM phusion/baseimage:focal-1.1.0 as dist
WORKDIR /opt/shaunwhite
COPY --from=build-env /opt/shaunwhite/shaunwhite ./shaunwhite
COPY token ./token

CMD ./shaunwhite --token ./token --config ./config.json
