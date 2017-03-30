FROM buildpack-deps:jessie-scm

MAINTAINER MATSUBARA Nobutada

WORKDIR ~

RUN echo 'deb http://ppa.launchpad.net/hvr/ghc/ubuntu trusty main' > /etc/apt/sources.list.d/ghc.list \
    && apt-key adv --keyserver keyserver.ubuntu.com --recv-keys F6F88286 \
    && apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates \
    g++ \
    ghc-8.0.2 \
    libtinfo-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*
RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 575159689BEFB442 \
    && echo 'deb http://download.fpcomplete.com/debian jessie main' | tee /etc/apt/sources.list.d/fpco.list \
    && apt-get update && apt-get install -y --no-install-recommends \
    stack \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*
RUN stack config set system-ghc --global true
ENV PATH /opt/ghc/8.0.2/bin:$PATH
