FROM ubuntu:22.04 AS builder

ENV SNAP_FIMEX_VERSION=1.9

RUN apt-get update -y && \
    apt-get install -y software-properties-common && \
    add-apt-repository -y ppa:met-norway/fimex && \
    apt-get update -y && \
    apt-get install -y libnetcdff-dev gfortran libfimex-$SNAP_FIMEX_VERSION-dev make && \
    apt-get remove -y software-properties-common && \
    apt-get -y autoremove && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /snap
ARG VERSION="latest"
ADD src .
RUN ln --symbolic --force gcc_pkgconfig.mk current.mk
RUN make clean && make

FROM ubuntu:22.04

ENV SNAP_FIMEX_VERSION=1.9

RUN apt-get update -y && \
    apt-get install -y software-properties-common && \
    add-apt-repository -y ppa:met-norway/fimex && \
    apt-get update -y && \
    apt-get install -y libnetcdff7 libfimex-$SNAP_FIMEX_VERSION-0 tini && \
    apt-get remove -y software-properties-common && \
    apt-get -y autoremove && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /snap
COPY --from=builder /snap/naccident/bsnap_naccident /snap/bsnap

ENTRYPOINT ["/usr/bin/env", "TINI_VERBOSITY=0", "/usr/bin/tini", "-g", "--", "/snap/bsnap"]
