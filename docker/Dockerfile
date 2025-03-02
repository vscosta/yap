# Use an official Python runtime as a parent image
FROM ubuntu AS yap

ENV TZ=Europe/Lisbon
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone



# Set the working directory to /app
WORKDIR /app

# the basics: must always be in the image
# MPI (MPich should also work
# python support (requires numpy)
# tools for yap4py
# tools for yapkernel
# gecode support
# R support
# RDF
RUN apt update && apt -y upgrade && \
  apt -y install git gcc g++ make cmake\
     autotools-dev automake autoconf perl-base m4 doxygen flex bison\
        libreadline-dev libgmp-dev \
    openmpi-bin libopenmpi-dev\
    swig ninja-build\
    python3 python3-dev python3-numpy    python3-numpy-dev \
     python3-pip   python3-wheel python3-setuptools\
    libgecode-dev \
    r-cran-rcpp \
    libraptor2-dev 

    

#yap binary

# BDD library
RUN  git clone https://github.com/ivmai/cudd.git /app/cudd \
    && cd /app/cudd \
    && aclocal\
    && automake\
    && ./configure --prefix=/usr --enable-shared --enable-obj --enable-dddmp&& make -j install&& cd ..

RUN  git clone https://github.com/vscosta/yap /app/yap --depth=3\
    && mkdir -p /app/yap/build\
    && cd /app/yap/build\
    && cmake .. -DWITH_DOXYGEN=1 -DCMAKE_BUILD_TYPE=Release -DWITH_PACKAGES=1 -DCMAKE_INSTALL_PREFIX:PATH=/usr -G Ninja \
    && ninja install

##RUN  mkdir -p /app/yap/build &&\
RUN     cd /app/yap/build &&\
     apt install -y mkdocs &&\
     ninja  mkdocs

#Python extras

RUN cd /app/yap/build/packages/python/yap4py \
    && python3 -m pip install --break-system-packages .

#R extras

RUN cd /app/yap/build \
    && cmake --build . --target YAP4R
RUN cd /app/yap/build \
    &&  R CMD INSTALL packages/real/yap4r


RUN cd /app/yap/build  &&\
cpack --config CPackConfig.cmake -G DEB                                             



#    Make port 80 available to the world outside this container
EXPOSE 22 80 8888

# Define environment variable
ENV NAME World

## Run app.py when the container launches
#CMD ["jupyter", "lab", "--port=8888", "--no-browser","--NotebookApp.token=''","--NotebookApp.password=''", "--ip=0.0.0.0", "--allow-root", "tut.ipynb" ]

