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

### 1. Start DB and Redis server

Install [docker](https://docs.docker.com/install/) and [docker-compose](https://docs.docker.com/compose/install/).

And type:

```
$ make infra
```

The DB and Redis server would open some available localhost ports.

Collect those ports and give them to env vars `DB_PORT` and `REDIS_PORT`.

You can get them by:

```
$ make ports
```

### 2. Start Rails web server

Install [Ruby 2.5.3](https://www.ruby-lang.org/en/news/2018/10/18/ruby-2-5-3-released/).

If you want to manage multiple versions of ruby, [rbenv](https://github.com/rbenv/rbenv) might help.

Then install `bundler`, which is something like `stack` in Haskell.

And move to `rb` dir, install dependent `gem`s, which are packages in Ruby.

```
$ ruby --version
ruby 2.5.3p105 (2018-10-18 revision 65156) [x86_64-darwin17]
$ gem install bundler
...
$ cd rb
$ bundle
```

For the first time, you need to setup database.

```
$ rails db:create db:migrate
```

Now it is ready to start a web server.

Make sure your current directory is back to the repository root.

```
$ cd ..
$ make web
```

Now server is running at `localhost:3000`.

### 3. Build and run Haskell code

Set env vars:

```
export DB_DATABASE=rb_development
export SIDEKIQ_NAMESPACE=sidekiq_rb_development
export DB_PORT=32770
export REDIS_PORT=32769
```

Port numbers written above are dummy. 
Give actual ones you opened at step 1.

Set the other vars as are.

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
