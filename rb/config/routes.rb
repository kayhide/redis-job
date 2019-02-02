Rails.application.routes.draw do
  resources :predictors

  require 'sidekiq/web'
  mount Sidekiq::Web => '/sidekiq'
end
