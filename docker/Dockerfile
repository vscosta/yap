# Use an official Python runtime as a parent image
FROM ubuntu:latest AS ubuntu-cudd

ENV TZ=Europe/Lisbon
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone


RUN mkdir -p /temp

# Set the working directory to /app
WORKDIR /temp

RUN apt update && apt -y install \
    # how can we distribute Linux without these packages?
    git gcc g++ make\
    # to compile cudd we need
    autotools-dev automake autoconf perl-base m4

RUN  git clone https://github.com/vscosta/cudd.git temp/cudd \
    && cd temp/cudd \
    && aclocal \
    && automake \
    && ./configure --prefix=/usr --enable-shared --enable-obj --enable-dddmp&& make -j install&& cd ..

FROM ubuntu-cudd AS ubuntu-dox

RUN apt -y install \
    bison flex cmake python3

RUN  git clone https://github.com/vscosta/doxygen.git temp/doxygen && cd temp/doxygen && mkdir -p build&& cd build&& cmake .. -DCMAKE_INSTALL_PREFIX:PATH=/usr && cmake --build . --target install&& cd ..


FROM ubuntu-dox AS ubuntu-dev

# Install any needed packages specified in requirements.txt
RUN apt -y install \
    # the basics: must always be in the image \
    libreadline-dev libgmp-dev \
    # other tools we use
    swig\
    # python support (requires numpy) \
    python3-dev    python3-numpy-dev \
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
    #openjdk-11-jdk-headless\
    # MPI (MPich should also work) \
    openmpi-bin libopenmpi-dev

FROM ubuntu-dev AS ubuntu-yapdev

RUN  git clone https://github.com/vscosta/yap temp/yap\
    && cd temp/yap\
    && mkdir -p build\
    && cd build\
    && cmake .. -DCMAKE_INSTALL_PREFIX:PATH=/usr -DWITH_DOCS=YES\
    && cmake --build . \
    && cmake --install .

#Python & extras
RUN cd temp/yap/build \
    && pip install apackages/python/yap4py\
    && pip install packages/python/yapkernel\
    &&  R CMD INSTALL packages/real/yap\
    && mv html ../../misc/tut.ipynb /app&& cd ../..

RUN cd temp/yap/build&& cmake --build . docs&& mv html /usr/share/docs/yap


# Make port 80 available to the world outside this container
EXPOSE 22 80 8888

# Define environment variable
ENV NAME World

# Run app.py when the container launches
CMD ["jupyter", "lab","tut.ipynb"]


