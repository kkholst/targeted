FROM r-base:3.6.3

RUN apt-get update && \
	apt-get install -y python3 python3-pip tmux mg git && \
	rm -rf /var/lib/apt/lists/*

RUN git clone https://github.com/kkholst/target /root/target && \
	cd /root/target; make init-submodules

RUN pip3 install cmake ninja

WORKDIR /root/target

CMD bash
