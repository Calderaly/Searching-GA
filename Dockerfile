FROM python:3.12-slim

ENV VIRTUAL_ENV=/opt/venv
WORKDIR /app

COPY app/ /app/
CMD ["python", "main.py"]

# Labeling image
LABEL maintainer="Fa Ainama Caldera S <faainamacaldera16@gmail.com>"
LABEL version="1.0"
LABEL description="A simple Python GA search application"