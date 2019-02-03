Rails.application.routes.draw do
  root to: 'predictors#index'
  resources :predictors, only: %i[index create destroy] do
    resource :start_train,
             only: %i[create],
             controller: :predictors,
             action: :start_train
  end

  require 'sidekiq/web'
  mount Sidekiq::Web => '/sidekiq'
end
