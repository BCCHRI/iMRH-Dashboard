
FROM rocker/r-ver:4.3.2

RUN apt-get update && apt-get install -y \
    libsqlite3-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    && apt-get clean

RUN mkdir /home/app/

# Copy the R scripts into the image
COPY src/* /home/app/
COPY data/iMRH_db.sqlite ./data/

# Install the required R packages
RUN R -e "source('install_packages.R')"

# Expose port 3838
EXPOSE 3838

# Run the app
CMD ["R", "-e", "shiny::runApp('.', host='0.0.0.0', port=3838)"]