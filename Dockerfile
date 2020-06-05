FROM r-base:4.0.0

WORKDIR /root

COPY ./ /gnlearn/

RUN apt-get update && apt-get dist-upgrade -y && apt-get autoremove -y \
&& apt-get install libssl-dev libxml2-dev libcurl4-openssl-dev git -y \
&& R -e "install.packages('devtools')" \
&& R -e "devtools::install_local('gnlearn')"

CMD ["R"]
