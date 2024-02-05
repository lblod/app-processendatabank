# Open Proces Huis (microservices stack)

Backend for keeping track of processes in LOD format as well as their original BPMN forms, based on the mu.semte.ch microservices stack.

This repository is a [mu-project](https://github.com/mu-semtech/mu-project), it includes the minimal set of services required to run the openproceshuis.

## Requirements and assumptions

This project was tested on Ubuntu 20.04, but should work on most systems that run docker and docker-compose. A linux based system is recommended, but we welcome any feedback you might have when running this system on macOS or windows.

- a recent version of [docker needs to be installed](https://docs.docker.com/get-docker/)
- a recent version of [docker-compose needs to be installed](https://docs.docker.com/compose/install/)
- some basic shell experience is recommended

## Getting started

1. make sure all [requirements](#Requirements-and-assumptions) are met
2. clone this repository

```bash
git clone https://github.com/MartijnBogaert/app-openproceshuis
```

3. run the project

```bash
cd /path/to/mu-project
```

```bash
docker-compose -f docker-compose.yml -f docker-compose.dev.yml up
```

You can shut down using `docker-compose stop` and remove everything using `docker-compose rm`.

## Overview of services

- [mu-identifier](https://github.com/mu-semtech/mu-identifier)
- [mu-dispatcher](https://github.com/mu-semtech/mu-dispatcher)
- [mu-cl-resources](https://github.com/mu-semtech/mu-cl-resources)
