dev:
	ghcid --command "stack ghci --ghci-options -fdiagnostics-color=always" --test "DevMain.run"
.PHONY: dev

watch:
	stack build --fast --file-watch
.PHONY: watch

migrate:
	cd rb && bundle exec rails db:create db:migrate
.PHONY: migrate

web:
	cd rb && bundle exec rails server
.PHONY: web

worker:
	cd rb && bundle exec sidekiq -C config/sidekiq.yml
.PHONY: worker

infra:
	docker-compose up
.PHONY: infra

envs:
	@echo "export DB_DATABASE=rb_development"
	@echo "export SIDEKIQ_NAMESPACE=sidekiq_rb_development"
	@echo "export DB_PORT=$$(docker port redis-job_db_1 | cut -d ':' -f 2)"
	@echo "export REDIS_PORT=$$(docker port redis-job_redis_1 | cut -d ':' -f 2)"
.PHONY: envs


db-cli:
	psql -U postgres -h localhost -p $$DB_PORT -d rb_development
.PHONY: db-cli

redis-cli:
	redis-cli -p $$REDIS_PORT
.PHONY: redis-cli
