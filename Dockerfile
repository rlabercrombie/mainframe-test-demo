FROM centos:centos7

# install yum package
RUN yum install -y gcc \
        gcc-devel \
        gcc-c++ \
        make \
        bison \
        flex \
        gmp-devel \
        curses-devel \
        postgresql-devel \
        postgresql-server \
        autoconf \
        automake \
        libtool \
        psql \
        libdb-devel

# install gnucobol - for some reason i havent been able to find the yum 
# installation for gnucobol, so using source install isntead.

COPY ./lib/gnucobol-3.1.tar.gz /gnucobol/gnucobol-3.1.tar.gz
WORKDIR /gnucobol
RUN tar xvfz gnucobol-3.1.tar.gz
RUN cd gnucobol-3.1 &&\
        ./configure &&\
        make install
RUN echo "/usr/local/lib" > /etc/ld.so.conf.d/myapp.conf &&\
        ldconfig

# install OCESQL

### The ADD instruction copies new files, directories or remote file URLs from <src> and adds them to the filesystem of the image at the path <dest>. 
### If you want to run another version or your own test, rewrite it as needed.

# COPY ./lib/Open-COBOL-ESQL-develop.tar.gz /ocesql/Open-COBOL-ESQL-develop.tar.gz
# WORKDIR /ocesql
# RUN tar xvfz Open-COBOL-ESQL-develop.tar.gz &&\
#  cd Open-COBOL-ESQL-develop &&\
#  ./configure --prefix=/usr/ &&\
#  make install &&\
#  cd / &&\
#  rm -rf Open-COBOL-ESQL-1.2.tar.gz

# ENTRYPOINT ["/bin/bash"]

# FROM ubuntu

# RUN apt-get update && apt-get -y install \
#     gnucobol3 \
#     build-essential \
#     g++ \
#     automake \
#     autoconf \
#     libtool\
#     libpq5 \
#     libpq-dev

#installing libtool using source because agt-get wasnt about to find version 2.4.6
# COPY ./lib/libtool-2.4.6.tar.gz  /libtool/libtool-2.4.6.tar.gz
# WORKDIR /libtool
# RUN tar xvfz libtool-2.4.6.tar.gz &&\
#         cd libtool-2.4.6 &&\
#         ./configure --prefix=/usr/local/libtool/2_4_6 &&\
#         make install

# Setup ocesql, which allows executing sql statements in cobol programs
# RUN ln -s /usr/bin/aclocal /usr/bin/aclocal-1.13
# RUN ln -s /usr/bin/automake /usr/bin/automake-1.13
# # RUN automake --add-missing
COPY ./lib/ocesql /ocesql
WORKDIR /ocesql
# RUN autoreconf --force --install &&\
#     ./configure &&\
#     make install
RUN export CPPFLAGS="-I/usr/pgsql/include"
RUN export LDFLAGS="-L/usr/pgsql/lib"
ENV COBCPY=/ocesql/copy
RUN ./configure --prefix=/usr/ &&\
		make install

RUN mv /usr/lib/libocesql.so /tmp/libocesql.so

WORKDIR /home

# # Keep the container running
CMD tail -f /dev/null