FROM rocker/binder:4.1.2
LABEL maintainer='Unknown'
USER root
COPY . ${HOME}
RUN chown -R ${NB_USER} ${HOME}
USER ${NB_USER}

RUN wget https://github.com/benmarwick/JPA_Palaeolithic_Projectiles/raw/master/DESCRIPTION 

RUN rm DESCRIPTION.1; exit 0