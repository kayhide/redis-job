# redis-job

This is a Haskell sample to read jobs enqueued from rails app.

The following setup is assumed:
- Redis as a queue
- Sidekiq to enque jobs
- PostgreSQL

This demonstrates:
- Connect to Redis server
- Read Jobs from Redis server
- Abstract Job datatype along with its arguments
- Run background worker
- Support retrying (wip)


## How to start

The following procedure hits `make` command a lot.

If you are interested in each specific command, just open and see `Makefile`.

If you are on NixOS, don't forget to enter Nix shell first, the `shell.nix` file should provive environment necessary to build everything in the repo.

### 1. Start DB and Redis server

Install [docker](https://docs.docker.com/install/) and [docker-compose](https://docs.docker.com/compose/install/).

And type:

```
$ make infra
```

The DB and Redis server would open some available localhost ports.

Collect those ports and give them to env vars `DB_PORT` and `REDIS_PORT`.


Including them, you can collect all necessary env vars by:

```
$ make envs
```

The following steps assume these vars are set.


### 2. Start Rails web server

Install [Ruby 2.5.3](https://www.ruby-lang.org/en/news/2018/10/18/ruby-2-5-3-released/).

If you want to manage multiple versions of ruby, [rbenv](https://github.com/rbenv/rbenv) might help.

Then install `bundler`, which is something like `stack` in Haskell.

And move to `rb` dir, install dependent `gem`s, which are packages in Ruby.

```
$ ruby --version
ruby 2.5.3p105 (2018-10-18 revision 65156) [x86_64-darwin17]
$ gem install bundler:1.17.1
...
$ cd rb
$ bundle
```

When it fails saying:

```
An error occurred while installing pg (1.1.4), and Bundler cannot continue.
Make sure that `gem install pg -v '1.1.4' --source 'https://rubygems.org/'` succeeds before bundling.

In Gemfile:
  pg
```

it means `libpg` is not found on your system.

To install it, do it by one of these commands:

```sh
$ brew install postgresql
$ apt-get install libpq-dev
$ yum install postgresql-devel

```

After successful `bundle`, you need to setup database.

```
$ bundle exec rails db:create db:migrate
```

Now it is ready to start a web server.

Make sure your current directory is back to the repository root.

```
$ cd ..
$ make web
```

Now server is running at `localhost:3000`.

### 3. Build and run Haskell code

Finally you are ready to hit `stack`:

```
$ stack build
...
$ stack exec -- redis-job
```

### 4. Test running jobs

Open your browser, click "Create Predictor" and click "Train".

This enqueues a TrainJob and Haskell code dequeues it.

You can click "Train" multiple times quickly, you will see that jobs are working in parallel.

Optionally you can give `SIDEKIQ_CONCURRENCY` and set the number the worker (default is 5).

## Tips

### Using ghcid

Using `ghcid`, you can automate edit, reload and run development cycle.

Try:

```
$ make dev
```

This will rerun `DevMain.run` every time you edit code.

### Running Ruby worker

Ruby worker can be started by:

```
$ make woker
```

The library, sidekiq, comes with a web frontend.

You can open `http://localhost:3000/sidekiq` and observe how it works.

### Redis client

You can access the redis server directly with `redis-cli` command.

When doing it you need to pass the right port number.

This command will do it:

```
$ make redis-cli
```

Same to the DB:

```
$ make db-cli
```

### Using direnv

[direnv](https://direnv.net/) will help to manage env vars.

You can set project specific env vars by putting `.envrc` file.

The env vars given by that file is only available when you are at that directory or its subdirectories.
