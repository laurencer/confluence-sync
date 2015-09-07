FROM samdoshi/haskell-stack:latest

RUN stack setup

ADD . /usr/src/build

RUN cd /usr/src/build && stack install
