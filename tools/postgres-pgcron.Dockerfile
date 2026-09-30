# PostgreSQL test image for the account-activity audit scheduler experiment.
#
# The base digest fixes PostgreSQL 17.10 and the package version fixes pg_cron
# 1.6.7.  Update both deliberately after reviewing the upstream release notes.
FROM docker.io/library/postgres@sha256:0af65001d05296a2ead57ac4a6412433d8913d1bb5d0c88435a7d1e1ee5cb04b

RUN apt-get update \
    && DEBIAN_FRONTEND=noninteractive apt-get install --yes --no-install-recommends \
      postgresql-17-cron=1.6.7-3.pgdg13+1 \
    && rm -rf /var/lib/apt/lists/*

# initdb copies this sample into every fresh test cluster.  Background-worker
# execution avoids a scheduler-owned TCP credential and has a bounded worker
# budget.  A production deployment owns its schedule and may choose different
# resource limits, but must retain the preload and database configuration.
RUN printf '%s\n' \
      "shared_preload_libraries = 'pg_cron'" \
      "cron.database_name = 'web_api_dev'" \
      "cron.timezone = 'UTC'" \
      "cron.use_background_workers = on" \
      "cron.max_running_jobs = 2" \
      "max_worker_processes = 10" \
      "timezone = 'UTC'" \
      >> /usr/share/postgresql/postgresql.conf.sample
