FROM rocker/r-ver:4.3.2

RUN apt-get update && apt-get install -y \
    libsqlite3-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/* 

RUN mkdir -p /home/app/src /home/app/data 

WORKDIR /home/app/

COPY src/ /home/app/src/
COPY data/ /home/app/data/

RUN R -e "source('src/utils/install_packages.R')"

EXPOSE 3838
CMD ["R", "-e", "shiny::runApp('app.R', host='0.0.0.0', port=3838)"]

