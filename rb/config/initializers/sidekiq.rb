host = ENV.fetch('REDIS_HOST', 'localhost')
port = ENV.fetch('REDIS_PORT', 6379)
url = ENV.fetch('REDIS_URL', "redis://#{host}:#{port.to_s}")
app_name = Rails.application.class.parent.name.underscore

Sidekiq.configure_client do |config|
  hash = {
    url: url,
    namespace: "sidekiq_#{app_name}_#{Rails.env}",
  }
  if size = ENV['REDIS_CLIENT_SIZE']
    hash[:size] = size.to_i
  end
   config.redis = hash
end

Sidekiq.configure_server do |config|
  hash = {
    url: url,
    namespace: "sidekiq_#{app_name}_#{Rails.env}",
  }
  if size = ENV['REDIS_SERVER_SIZE']
    hash[:size] = size.to_i
  end
  config.redis = hash
end
