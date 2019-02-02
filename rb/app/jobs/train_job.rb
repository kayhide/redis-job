class TrainJob < ApplicationJob
  queue_as :default

  def perform(predictor)
    training_time = rand * 10
    test_time = training_time * rand

    sleep training_time
    predictor.update trained_at: Time.current

    sleep test_time
    predictor.update tested_at: Time.current
  end
end
