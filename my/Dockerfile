FROM amd64/ubuntu:jammy

RUN apt-get update && apt-get install -y \
  build-essential \
  cmake \
  gcc \
  gdb \
  git \
  time \
  vim \
  opam

opam init --disable-sandboxing -y

opam switch create 4.14.0

eval $(opam env --switch=4.14.0)
