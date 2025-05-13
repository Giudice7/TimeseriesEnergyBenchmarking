FROM rocker/verse:4.2.3

RUN apt-get update && apt-get install -y locales && \
    locale-gen en_US.UTF-8 && \
    update-locale LANG=en_US.UTF-8
ENV LANG=en_US.UTF-8
ENV LANGUAGE=en_US:en
ENV LC_ALL=en_US.UTF-8

RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/ && \
    echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" | \
    tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site

RUN R -e 'install.packages("remotes")'

# Install specific versions
RUN Rscript -e 'remotes::install_version("jsonlite", version = "1.8.4", upgrade = "never")'
RUN Rscript -e 'remotes::install_version("magrittr", version = "2.0.3", upgrade = "never")'
RUN Rscript -e 'remotes::install_version("shiny", version = "1.7.4", upgrade = "never")'
RUN Rscript -e 'remotes::install_version("config", version = "0.3.1", upgrade = "never")'
RUN Rscript -e 'remotes::install_version("testthat", version = "3.1.8", upgrade = "never")'
RUN Rscript -e 'remotes::install_version("spelling", version = "2.2.1", upgrade = "never")'
RUN Rscript -e 'remotes::install_version("shinyWidgets", version = "0.8.0", upgrade = "never")'
RUN Rscript -e 'remotes::install_version("shinyjs", version = "2.1.0", upgrade = "never")'
RUN Rscript -e 'remotes::install_version("shinydashboard", version = "0.7.2", upgrade = "never")'
RUN Rscript -e 'remotes::install_version("shinycssloaders", version = "1.0.0", upgrade = "never")'
RUN Rscript -e 'remotes::install_version("lubridate", version = "1.9.2", upgrade = "never")'
RUN Rscript -e 'remotes::install_version("golem", version = "0.4.1", upgrade = "never")'
RUN Rscript -e 'remotes::install_version("dplyr", version = "1.1.2", upgrade = "never")'
RUN Rscript -e 'remotes::install_github("twitter/BreakoutDetection@7ae3fc49001f8c3f5f46c0b33c61f90e1e889df6")'

# Copy app source and install
COPY . /app
WORKDIR /app
RUN Rscript -e 'remotes::install_local("/app", upgrade = "never", force = TRUE)'

EXPOSE 80
CMD ["R", "-e", "options(shiny.port=80, shiny.host='0.0.0.0'); library(TimeseriesEnergyBenchmarking); TimeseriesEnergyBenchmarking::run_app()"]
