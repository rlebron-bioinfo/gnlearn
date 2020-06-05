FROM r-base:4.0.0

WORKDIR /root

RUN apt-get update && apt-get dist-upgrade -y && apt-get autoremove -y \
&& apt-get install libssl-dev libxml2-dev libcurl4-openssl-dev git -y \
&& git clone https://github.com/gherardovarando/nodag /root/nodag \
&& R CMD SHLIB /root/nodag/nodag.f -llapack -lblas \
&& R -e "install.packages('devtools')" \
&& R -e "devtools::install_github('rlebron-bioinfo/gnlearn')"

CMD ["R"]
