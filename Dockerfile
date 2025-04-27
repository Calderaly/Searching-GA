FROM python:3.12-slim

# Install necessary packages (including Common Lisp and Supervisor)
RUN apt-get update && apt-get install -y --no-install-recommends \
    sbcl \
    supervisor \
    && rm -rf /var/lib/apt/lists/*

ENV VIRTUAL_ENV=/opt/venv
RUN python3 -m venv $VIRTUAL_ENV
ENV PATH="$VIRTUAL_ENV/bin:$PATH"
WORKDIR /app

COPY app/ /app/

# Create Supervisor configuration file
RUN mkdir -p /etc/supervisor/conf.d/
COPY supervisor.conf /etc/supervisor/conf.d/supervisor.conf

# Set the command to run Supervisor
CMD ["/usr/bin/supervisord", "-c", "/etc/supervisor/supervisord.conf"]

# Labeling image
LABEL maintainer="Fa Ainama Caldera S <faainamacaldera16@gmail.com>"
LABEL version="1.0"
LABEL description="A simple Python and Common Lisp (Steel Bank) GA search application"

#FROM python:3.12-slim
# Install Common Lisp
#RUN apt-get update && apt-get install -y --no-install-recommends sbcl && rm -rf /var/lib/apt/lists/*
#ENV VIRTUAL_ENV=/opt/venv
#RUN python3 -m venv $VIRTUAL_ENV
#ENV PATH="$VIRTUAL_ENV/bin:$PATH"
#WORKDIR /app
#COPY app/ /app/
# Run Python in the foreground and Lisp in the background (less ideal)
#CMD python main.py & sbcl --load main.lsp
# Labeling image
#LABEL maintainer="Fa Ainama Caldera S <faainamacaldera16@gmail.com>"
#LABEL version="1.0"
#LABEL description="A simple Python and Common Lisp (Steel Bank) GA search application"