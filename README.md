# Open Proces Huis (microservices stack)

This repo holds the docker compose and all configuration files necessary to get Open Proces Huis running. It was started using the [mu-project template](https://github.com/mu-semtech/mu-project) and has since been heavily expanded.

Open Proces Huis is one of many applications developed under the [Agentschap Binnenlands Bestuur (ABB)](https://www.vlaanderen.be/agentschap-binnenlands-bestuur), which is part of the [Flemish Government](https://www.vlaanderen.be/en). It allows for _lokale besturen_ to create and share processes. Such process is created by uploading a BPMN file, which is subsequently stored by the [file-service](https://github.com/mu-semtech/file-service), as well as processed by the [bpmn-service](https://github.com/lblod/bpmn-service). The latter was developed as part of Open Proces Huis and essentially extracts the BPMN elements from a given BPMN file and stores them as triples in the [virtuoso triplestore](https://github.com/tenforce/docker-virtuoso). To make all functionalities available as a web application, a [frontend](https://github.com/lblod/frontend-openproceshuis) was also developed.

In addition to the aforementioned services, a range of others are also essential to the stack. All of them are listed in [this overview](#overview-of-services), and can of course also be found in [docker-compose.yml](./docker-compose.yml).

## Getting started

### First run

1. Clone this repository

```bash
git clone https://github.com/lblod/app-openproceshuis.git
```

2. Run the project

```bash
cd /path/to/project
```

```bash
docker compose -f docker-compose.yml -f docker-compose.dev.yml up
```

3. Wait for the op-consumer to finish initial ingest

```bash
docker compose logs -f op-consumer
```

> When the logs show `delta-sync-queue: Remaining number of tasks 0`, you can move on.

4. In your browser, go to [localhost:8890/sparql](http://localhost:8890/sparql) and run the SPARQL query found in [manual-query-reasoning-service.sparql](./manual-query-reasoning-service.sparql).

### Usage

- You can access the frontend in your browser by going to [localhost](http://localhost/).
- You can log in using a mock account by going to [localhost/mock-login](http://localhost/mock-login).
- You can shut everything down by running `docker compose down`.
- When restarting the project not having emptied the `data/` folder, you can ignore steps 3 and 4 under [First run](#first-run).
- You can empty the database and file storage by running `sudo rm -rf data/` (restarting the project will require steps 3 and 4 under [First run](#first-run)).

## Overview of services

- [frontend-openproceshuis](https://github.com/lblod/frontend-openproceshuis)
- [mu-identifier](https://github.com/mu-semtech/mu-identifier)
- [mu-dispatcher](https://github.com/mu-semtech/mu-dispatcher)
- [mu-authorization](https://github.com/mu-semtech/mu-authorization)
- [virtuoso](https://github.com/tenforce/docker-virtuoso)
- [mu-migrations-service](https://github.com/mu-semtech/mu-migrations-service)
- [mu-cl-resources](https://github.com/mu-semtech/mu-cl-resources)
- [mu-cache](https://github.com/mu-semtech/mu-cache)
- [delta-notifier](https://github.com/mu-semtech/delta-notifier)
- [file-service](https://github.com/mu-semtech/file-service)
- [bpmn-service](https://github.com/lblod/bpmn-service)
- [account-detail-service](https://github.com/lblod/account-detail-service)
- [mu-search](https://github.com/mu-semtech/mu-search)
- [mu-search-elastic-backend](https://github.com/mu-semtech/mu-search-elastic-backend)
- [delta-consumer](https://github.com/lblod/delta-consumer)
- [reasoning-service](https://github.com/eyereasoner/reasoning-service)
- [acmidm-login-service](https://github.com/lblod/acmidm-login-service)
