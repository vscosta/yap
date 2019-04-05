# Use an official Python runtime as a parent image
FROM ubuntu:18.04

# Update Software repository
RUN apt-get update
 

# Set the working directory to /app
WORKDIR /app

# Copy the current directory contents into the container at /app
COPY . /app

# Install any needed packages specified in requirements.txt
RUN apt install gcc g++ libreadline-dev libgmp-dev git \
    python3-dev python3-pip  python3-notebook python3-wheel python3-setuptools\
    libgecode-dev r-cran-rcpp cmake libbdd-dev libxml2-dev \
    bison flex openjdk-11-jdk-headless libraptor2-dev swig

#RUN git clone ssh://vsc@ssh.dcc.fc.up.pt:31064/yap.git
RUN git clone https://github.com/vscosta/cudd.git
RUN git clone https://github.com/vscosta/doxygen-yap.git
RUN git clone https://github.com/vscosta/cudd.git

RUN pushd cudd; ./configure --prefix=/usr --enable-shared --enable-obj --enable-dddmp; make -j install; popd

RUN pushd doxygen-yap; mkdir -p build; cd build; cmake .. -DCMAKE_INSTALL_PREFIX:PATH=/usr && cmake --build . --target install; popd

RUN pushd yap; mkdir -p build; cd build; cmake .. -DCMAKE_INSTALL_PREFIX:PATH=/usr && cmake --build . --target install; pushd paackages/real; R CMD INSTALL yap4r; popd; popd

RUN R CMD INSTALL packages/real/yap4r

# Make port 80 available to the world outside this container
EXPOSE 80

# Define environment variable
ENV NAME World

# Run app.py when the container launches
CMD ["python", "app.py"]
This Dockerfile refers to a couple of files we haven’t created yet, namely app.py and requirements.txt. Let’s create those next.

