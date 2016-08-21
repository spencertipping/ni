FROM ubuntu:10.04
# RUN apt-get update
# RUN apt-get install -y less
ADD ni /usr/bin/
VOLUME /data
WORKDIR /data
CMD /bin/bash
