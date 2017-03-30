FROM buildpack-deps:jessie-scm

MAINTAINER MATSUBARA Nobutada

WORKDIR ~

RUN apt-get update && apt-get install -y --no-install-recommends \
    g++ \
    gcc \
    make \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*
RUN git clone https://github.com/rbenv/rbenv.git ~/.rbenv
RUN cd ~/.rbenv \
    && src/configure \
    && make -C src
RUN git clone https://github.com/sstephenson/ruby-build.git ~/.rbenv/plugins/ruby-build
RUN echo 'export PATH="$HOME/.rbenv/bin:$PATH"' >> ~/.bashrc
