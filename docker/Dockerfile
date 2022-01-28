# Use an official Python runtime as a parent image
FROM ubuntu AS yap

ENV TZ=Europe/Lisbon
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone



# Set the working directory to /app
WORKDIR /app

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

RUN  git clone https://github.com/vscosta/cudd.git /app/cudd \
    && cd /app/cudd \
    && aclocal\
    && automake\
    && ./configure --prefix=/usr --enable-shared --enable-obj --enable-dddmp&& make -j install&& cd ..

RUN  git clone https://github.com/vscosta/yap /app/yap --depth=6\
    && mkdir -p /app/yap/build\
    && cd /app/yap/build\
    && cmake .. -DWITH_DOXYGEN=1  -DWITH_PACKAGES=1 -DCMAKE_INSTALL_PREFIX:PATH=/usr \
    && cmake --build . --parallel --target install


RUN  git clone https://github.com/vscosta/doxygen-yap.git /app/doxygen-yap &&\
    cd /app/doxygen-yap &&\
    mkdir -p build&&\
    cd build&&\
    cmake .. -DCMAKE_INSTALL_PREFIX:PATH=/usr && cmake --build . --target install --parallel

RUN  mkdir -p /app/yap/build && \
    cd /app/yap/build &&\
    doxygen-yap  Doxyfile.dox 

#Python extras
RUN apt-get -y install curl \
    && curl -fsSL https://deb.nodesource.com/setup_17.x|bash -\
    &&  apt-get install -y nodejs\
    && cd /app/yap/build/packages/python/yap4py \
    && pip3 install jupyterlab\
    && pip3 install .\
    && cd /app/yap/packages/python/yapkernel \
      && pip3 install  .\
    && python3 -m yapkernel.kernelspec


RUN python3 -m jupyter lab build\
    &&mkdir /usr/local/share/jupyter/lab/staging/node_modules/codemirror/mode/prolog\
    &&cp /app/yap/misc/editors/codemirror/meta.js /usr/local/share/jupyter/lab/staging/node_modules/codemirror/mode/meta.js\
    &&cp /app/yap/misc/editors/codemirror/prolog.js /usr/local/share/jupyter/lab/staging/node_modules/codemirror/mode/prolog\
    &&python3 -m jupyter lab build\
    &&cp /app/yap/misc/tut.ipynb /app


#R extras
RUN cd /app/yap/build \
    && cmake --build . --target YAP4R
RUN cd /app/yap/build \
    &&  R CMD INSTALL packages/real/yap4r

# Make port 80 available to the world outside this container
EXPOSE 22 80 8888

# Define environment variable
ENV NAME World

# Run app.py when the container launches
CMD ["jupyter", "lab", "--port=8888", "--no-browser","--NotebookApp.token=''","--NotebookApp.password=''", "--ip=0.0.0.0", "--allow-root", "tut.ipynb" ]

