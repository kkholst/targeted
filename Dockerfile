FROM r-base:3.6.3

RUN apt-get update && \
	apt-get install -y python3 python3-pip tmux mg git && \
	rm -rf /var/lib/apt/lists/*

RUN pip3 install cmake ninja

COPY build/export/target /root/target

WORKDIR /root/target

CMD bash
