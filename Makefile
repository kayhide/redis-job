dev:
	$$(make envs) && ghcid --command "stack ghci redis-job:lib redis-job:exe:dev --ghci-options -fdiagnostics-color=always" --test "Main.main"
.PHONY: dev

watch:
	stack build --fast --file-watch
.PHONY: watch

migrate:
	$$(make envs) && cd rb && bundle exec rails db:create db:migrate
.PHONY: migrate

web:
	$$(make envs) && cd rb && bundle exec rails server
.PHONY: web

worker:
	$$(make envs) && cd rb && bundle exec sidekiq -C config/sidekiq.yml
.PHONY: worker

infra-up:
	docker-compose up -d
.PHONY: infra-up

infra-down:
	docker-compose down
.PHONY: infra-down


envs:
	@echo "export DB_DATABASE=rb_development"
	@echo "export SIDEKIQ_NAMESPACE=sidekiq_rb_development"
	@echo "export DB_PORT=$$(docker port $$(docker ps -q --filter 'name=redis-job_db_*') | cut -d ':' -f 2)"
	@echo "export REDIS_PORT=$$(docker port $$(docker ps -q --filter 'name=redis-job_redis_*') | cut -d ':' -f 2)"
.PHONY: envs


db-cli:
	psql -U postgres -h localhost -p $$DB_PORT -d rb_development
.PHONY: db-cli

redis-cli:
	redis-cli -p $$REDIS_PORT
.PHONY: redis-cli
