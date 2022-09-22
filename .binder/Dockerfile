FROM rocker/binder:4.1.2
LABEL maintainer='Unknown'
USER root
COPY . ${HOME}
RUN chown -R ${NB_USER} ${HOME}
USER ${NB_USER}

RUN wget https://github.com/benmarwick/JPA_Palaeolithic_Projectiles/raw/master/DESCRIPTION && R -e "options(repos = list(CRAN = 'http://mran.revolutionanalytics.com/snapshot/2022-09-22/')); devtools::install_deps()"
RUN rm DESCRIPTION.1; exit 0
",
      .open = "[",
      .close = "]"
    ) -> docker_contents
    path <- sanitize_path(path) # To kill trailing slashes
    fs::dir_create(glue("{path}/.binder"))
    fileConn <- file(glue("{path}/.binder/Dockerfile"))
    writeLines(docker_contents, fileConn)
    close(fileConn)

    cliapp::cli_alert_success(glue("Dockerfile generated at {path}/.binder/Dockerfile"))
  }


RUN rm DESCRIPTION.1; exit 0