FROM ubuntu:10.04
ADD ni /usr/bin/
VOLUME /data
WORKDIR /data
CMD /bin/bash
