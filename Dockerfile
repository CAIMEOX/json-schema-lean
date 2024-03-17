FROM caimeox/lean4:latest

WORKDIR /app

COPY . /app
RUN lake build
CMD ["./.lake/build/bin/bowtie"]