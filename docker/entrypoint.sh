#!/bin/bash
# Entry point of the dev container: load the Spack environment (ww3-gnu) and the
# WW3 build variables written at image build time (docker/Dockerfile step 4),
# then run the command.
. /opt/ww3-env.sh
exec "$@"
