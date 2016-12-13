FROM unikernel/mirage
RUN sudo apt-get update
RUN opam depext -y conf-autoconf
RUN opam pin add -y pih-store git://github.com/ucn-eu/pih-store