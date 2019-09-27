# Use an official Python runtime as a parent image
FROM ubuntu:18.04

ENV TZ=Europe/Lisbon
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone

# Update Software repository
RUN apt-get -y update 
RUN apt -y dist-upgrade

 

# Set the working directory to /app
WORKDIR /app

# Copy the current directory contents into the container at /app
COPY . /app

# Install any needed packages specified in requirements.txt
RUN apt -y install gcc g++ libreadline-dev libgmp-dev git \
    python3-dev python3-pip  python3-notebook python3-wheel python3-setuptools\
    libgecode-dev r-cran-rcpp cmake libbdd-dev libxml2-dev \
    bison flex openjdk-11-jdk-headless libraptor2-dev swig

#RUN git clone ssh://vsc@ssh.dcc.fc.up.pt:31064/yap.git
RUN git clone https://github.com/vscosta/cudd.git cudd
RUN git clone https://github.com/vscosta/doxygen-yap.git doxygen
RUN git clone https://github.com/vscosta/yap-6.3.git yap


RUN cd cudd; ./configure --prefix=/usr --enable-shared --enable-obj --enable-dddmp; make -j install; cd ..

RUN cd doxygen; mkdir -p build; cd build; cmake .. -DCMAKE_INSTALL_PREFIX:PATH=/usr && cmake --build . --target install; cd ..

RUN cd yap; mkdir -p build; cd build; cmake .. -DCMAKE_INSTALL_PREFIX:PATH=/usr -DWITH_DOCS=1 && cmake --build . --target install; cd packages/real; R CMD INSTALL yap4r; cd ../../docs;  doxygen-yap; mv html ../../misc/tut.ipynb /app; cd ../..


# Make port 80 available to the world outside this container
EXPOSE 80 8888

# Define environment variable
ENV NAME World

# Run app.py when the container launches
CMD ["jupyter", "lab","tut.ipynb"]


