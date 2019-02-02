Rails.application.routes.draw do
  root to: "predictors#index"
  resources :predictors do
    resource :start_train, only: [:create], controller: "predictors", action: "start_train"
  end

  require 'sidekiq/web'
  mount Sidekiq::Web => '/sidekiq'
end
