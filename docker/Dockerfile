# Use an official Python runtime as a parent image
FROM ubuntu AS yap

ENV TZ=Europe/Lisbon
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone


RUN mkdir -p /temp

# Set the working directory to /app
WORKDIR /temp

RUN apt update && apt -y upgrade && apt -y install \
    # how can we distribute Linux without these packages?\
    git gcc g++ make cmake\
    # to compile cudd we need\
    autotools-dev automake autoconf perl-base m4 doxygen flex bison\
    # the basics: must always be in the image \
    libreadline-dev libgmp-dev\
    # MPI (MPich should also work) \
    openmpi-bin libopenmpi-dev\
    # other tools we use\
    swig\
    # python support (requires numpy) \
    python3 python3-dev python3-numpy    python3-numpy-dev \
    # tools for yap4py \
    python3-pip   python3-wheel python3-setuptools\
    # tools for yapkernel \
    python3-notebook \
    # gecode support \
    libgecode-dev \
    # R support \
    r-cran-rcpp \
    # XML and RDF \
    libxml2-dev  libraptor2-dev \
    openjdk-11-jdk-headless




#yap binary

RUN  git clone https://github.com/vscosta/cudd.git /temp/cudd \
    && cd /temp/cudd \
    && aclocal\
    && automake\
    && ./configure --prefix=/usr --enable-shared --enable-obj --enable-dddmp&& make -j install&& cd ..

RUN  git clone https://github.com/vscosta/yap /temp/yap --depth=6\
    && mkdir -p /temp/yap/build\
    && cd /temp/yap/build\
    && cmake .. -DWITH_DOXYGEN=1  -DWITH_PACKAGES -DCMAKE_INSTALL_PREFIX:PATH=/usr \
    && cmake --build . --parallel --target install


RUN  git clone https://github.com/vscosta/doxygen-yap.git /temp/doxygen &&\
    cd /temp/doxygen &&\
    mkdir -p build&&\
    cd build&&\
    cmake .. -DCMAKE_INSTALL_PREFIX:PATH=/usr && cmake --build . --target install --parallel

RUN  mkdir -p /temp/yap/build && \
    cd /temp/yap/build &&\
    doxygen-yap  Doxyfile.dox 

#Python extras
RUN  cd /temp/yap/build\
    && pip3 install  .\
    && cd /temp/yap/packages/python/yapkernel \
    && pip3 install jupyterlab\
    && pip3 install .\
    && python3 -m yapkernel.kernelspec\
    && apt -y install  npm\
    && python3 -m jupyter lab build\
    &&mkdir /usr/local/share/jupyter/lab/staging/node_modules/codemirror/mode/prolog\
    &&cp ../misc/editors/codemirror/meta.js /usr/local/share/jupyter/lab/staging/node_modules/codemirror/mode/meta.js\
    &&cp ../misc/editors/codemirror/prolog.js /usr/local/share/jupyter/lab/staging/node_modules/codemirror/mode/prolog\
    &&python3 -m jupyter lab build


#R extras
RUN cd /temp/yap/build \
    && cmake --build . --target YAP4R
RUN cd /temp/yap/build \
    &&  R CMD INSTALL packages/real/yap4r

RUN cd temp/yap/build&& cmake --build . --target docs\
    && mkdir -p /usr/share/docs/yap\
    && mv html /usr/share/docs/yap


# Make port 80 available to the world outside this container
EXPOSE 22 80 8888

# Define environment variable
ENV NAME World

# Run app.py when the container launches
CMD ["jupyter", "lab","tut.ipynb"]


