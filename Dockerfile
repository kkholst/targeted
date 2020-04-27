FROM kkholst/stat:base

RUN install_packages tmux mg git && \
    pip3 install cmake ninja

COPY build/export /root/target

WORKDIR /root/target

CMD bash
