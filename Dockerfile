FROM unikernel/mirage

ENV DEBIAN_FRONTEND noninteractive
RUN sudo apt-get update
RUN opam depext -y conf-autoconf conf-gmp


#if COPY to /src, chown won't work
COPY . /home/opam/pih-store
RUN opam pin add pih-store /home/opam/pih-store
RUN opam install nocrypto mirage-logs


RUN sudo chown -R opam:opam /home/opam/pih-store/template


#better use absolute path, if not the first WORKDIR, the argument
#will be understood as a relative path to previously set path
WORKDIR /home/opam/pih-store/template
RUN opam config exec -- mirage clean
RUN opam config exec -- mirage configure -t unix --no-opam
RUN opam config exec -- mirage build


#if multiple CMDs provided, only the last one will be executed
EXPOSE 8080
#ENTRYPOINT ["./mir-review &"]

