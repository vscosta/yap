# Use an official Python runtime as a parent image
FROM ubuntu AS yap

ENV TZ=Europe/Lisbon
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone



# Set the working directory to /app
WORKDIR /app

RUN apt update && apt -y upgrade  \
    # how can we distribute Linux without these packages?\
    && apt -y install git gcc g++ make cmake\
    # to compile cudd we need\
    && apt -y install autotools-dev automake autoconf perl-base m4 doxygen flex bison\
    # the basics: must always be in the image \
    && apt -y install libreadline-dev libgmp-dev\
    # MPI (MPich should also work) \
    && apt -y install openmpi-bin libopenmpi-dev\
    # other tools we use\
    && apt -y install swig\
    # python support (requires numpy) \
    && apt -y install python3 python3-dev python3-numpy    python3-numpy-dev \
    # tools for yap4py \
    && apt -y install python3-pip   python3-wheel python3-setuptools\
    # tools for yapkernel \
    #python3-notebook \
    # gecode support \
    && apt -y install libgecode-dev \
    # R support \
    && apt -y install r-cran-rcpp \
    # XML and RDF \
    && apt -y install  libraptor2-dev \

#yap binary

# BDD library
RUN  git clone https://github.com/vscosta/cudd.git /app/cudd \
    && cd /app/cudd \
    && aclocal\
    && automake\
    && ./configure --prefix=/usr --enable-shared --enable-obj --enable-dddmp&& make -j install&& cd ..

RUN  git clone https://github.com/vscosta/yap /app/yap --depth=6\
    && mkdir -p /app/yap/build\
    && cd /app/yap/build\
    && cmake .. -DWITH_DOXYGEN=1 -DCMAKE_BUILD_TYPE=Release -DWITH_PACKAGES=1 -DCMAKE_INSTALL_PREFIX:PATH=/usr -G Ninja \
    && cmake --build . --parallel --target install

RUN 


# RUN  git clone https://github.com/vscosta/doxygen-yap.git /app/doxygen-yap &&\
#     cd /app/doxygen-yap &&\
#     mkdir -p build&&\
#     cd build&&\
#     cmake .. -DCMAKE_INSTALL_PREFIX:PATH=/usr && cmake --build . --target install --parallel -G Ninja

# RUN  mkdir -p /app/yap/build && \
#     cd /app/yap/build &&\
#     doxygen-yap  Doxyfile.dox 

# RUN cd /app/yap/build 
cpack --config CPackConfig.cmake
#R extras

# RUN cd /app/yap/build \
#     && cmake --build . --target YAP4R
# RUN cd /app/yap/build \
#     &&  R CMD INSTALL packages/real/yap4r

# Make port 80 available to the world outside this container
EXPOSE 22 80 8888

# Define environment variable
ENV NAME World

## Run app.py when the container launches
#CMD ["jupyter", "lab", "--port=8888", "--no-browser","--NotebookApp.token=''","--NotebookApp.password=''", "--ip=0.0.0.0", "--allow-root", "tut.ipynb" ]

