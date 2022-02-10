# Use an official Python runtime as a parent image
FROM ubuntu AS yap-srl

ENV TZ=Europe/Lisbon
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone



# Set the working directory to /srl
WORKDIR /srl
WORKDIR /srl/cudd
WORKDIR /srl/data

WORKDIR /srl/data
RUN apt update && apt -y upgrade && apt -y install \
    # how can we distribute Linux without these packages?\
    git gcc g++ make cmake\
    # to compile cudd we need\
    autotools-dev automake autoconf perl-base m4 doxygen flex bison\
    # the basics: must always be in the image \
    libreadline-dev libgmp-dev\
    # MPI (MPich should also work) \
    openmpi-bin libopenmpi-dev\
    libgecode-dev 
    # R support 

#yap binary

RUN  git clone https://github.com/vscosta/cudd.git /srl/cudd \
    && cd /srl/cudd \
    && aclocal\
    && automake\
    && ./configure --prefix=/usr --enable-shared --enable-obj --enable-dddmp&& make -j install&& cd ..

RUN  git clone https://github.com/vscosta/yap /srl/yap --depth=6\
    && mkdir -p /srl/yap/build\
    && cd /srl/yap/build\
    && cmake ..  -DWITH_PACKAGES=0 -DWITH_BDD=1 -DWITH_PROBLOG=1 -DWITH_GECODE=1 -DWITH_CLPBN=1 -DCMAKE_INSTALL_PREFIX:PATH=/usr \
    && cmake --build . --parallel --target install

RUN mkdir -p /srl/data 
COPY ilp/UPAleph  /srl/UPAleph      
COPY ilp/skill_base  /srl/skill_base
COPY ilp/chemo_js  /srl/data/chemo_js
COPY ilp/uw-cse  /srl/data/uw-cse



# Make port 80 available to the world outside this container
EXPOSE 22 

CMD ["yap", "-l", "/srl/UPAleph/aleph"]