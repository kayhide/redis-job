class PredictorsController < ApplicationController
  before_action :set_predictor, only: %i[destroy start_train]

  def index
    @predictors = Predictor.all
  end

  def create
    @predictor = Predictor.create training_set: '', train_net: '', test_set: '', predict_net: ''
    redirect_to predictors_url, notice: 'Predictor is created.'
  end

  def destroy
    @predictor.destroy
    redirect_to predictors_url, notice: 'Predictor is destroyed.'
  end

  def start_train
    TrainJob.perform_later @predictor
    redirect_to predictors_url, notice: 'Train job is submitted.'
  end

  private

  def set_predictor
    @predictor = Predictor.find(params[:id] || params[:predictor_id])
  end
end
