FROM rocker/r-ver:4.3.2

RUN apt-get update && apt-get install -y \
    libsqlite3-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/* 

RUN mkdir -p /home/app/src /home/app/data

# Set working directory to the Shiny app source
WORKDIR /home/app/src

# Copy application source (token file is excluded via .dockerignore)
COPY src/ /home/app/src/

# Copy static data
COPY data/ /home/app/data/

# Install required R packages
RUN R -e "source('utils/install_packages.R')"

# Expose Shiny port
EXPOSE 3838

# Launch the app (reads iMRHToken from working directory at runtime)
CMD ["R", "-e", "shiny::runApp('.', host='0.0.0.0', port=3838)"]
