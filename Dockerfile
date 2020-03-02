FROM kkholst/stat:base

RUN pip install scipy patsy pandas statsmodels 
RUN R -e 'install.packages(c("lava", "DEoptim"), repos="https://cloud.r-project.org/")'
RUN apk add tmux mg git ninja ccache

ENV CCACHE_DIR=/tmp/ccache
WORKDIR /data
CMD make init test
#r py clean
