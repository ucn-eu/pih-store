FROM unikernel/mirage

ENV DEBIAN_FRONTEND noninteractive
RUN sudo apt-get update
RUN opam depext -y conf-autoconf conf-gmp

COPY . /src/pih-store
RUN opam pin add pih-store /src/pih-store
RUN opam install nocrypto
RUN cd /src/pih-store/template

RUN opam config exec -- mirage clean
RUN opam config exec -- mirage configure -t unix --no-opam
RUN opam config exec -- mirage build

EXPOSE 8080
WORKDIR /src/pih-store/template
CMD ["./mir-review"]

