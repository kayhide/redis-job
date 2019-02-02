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
- Run background worker (wip)
- Support retrying (wip)

