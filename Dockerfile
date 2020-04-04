FROM kkholst/stat:default

RUN     R -e 'install.packages(c("lava", "DEoptim"), repos="https://cloud.r-project.org/")'

RUN	pip3 install --no-binary :all: --upgrade-strategy only-if-needed \
	patsy statsmodels==0.10.2 && \
	rm -Rf /tmp/* /root/.cache

RUN	apk add --no-cache tmux mg git  && \ 
 	rm -Rf /tmp/* /root/.cache /var/cache/apk/*

ENV CCACHE_DIR=/tmp/ccache
WORKDIR /data

CMD make init test
