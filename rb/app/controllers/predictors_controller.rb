class PredictorsController < ApplicationController
  before_action :set_predictor, only: %i[show edit update destroy start_train]

  # GET /predictors
  # GET /predictors.json
  def index
    @predictors = Predictor.all
  end

  # GET /predictors/1
  # GET /predictors/1.json
  def show
  end

  # GET /predictors/new
  def new
    @predictor = Predictor.new
  end

  # GET /predictors/1/edit
  def edit
  end

  # POST /predictors
  # POST /predictors.json
  def create
    @predictor = Predictor.new(predictor_params)

    respond_to do |format|
      if @predictor.save
        format.html { redirect_to @predictor, notice: 'Predictor was successfully created.' }
        format.json { render :show, status: :created, location: @predictor }
      else
        format.html { render :new }
        format.json { render json: @predictor.errors, status: :unprocessable_entity }
      end
    end
  end

  # PATCH/PUT /predictors/1
  # PATCH/PUT /predictors/1.json
  def update
    respond_to do |format|
      if @predictor.update(predictor_params)
        format.html { redirect_to @predictor, notice: 'Predictor was successfully updated.' }
        format.json { render :show, status: :ok, location: @predictor }
      else
        format.html { render :edit }
        format.json { render json: @predictor.errors, status: :unprocessable_entity }
      end
    end
  end

  # DELETE /predictors/1
  # DELETE /predictors/1.json
  def destroy
    @predictor.destroy
    respond_to do |format|
      format.html { redirect_to predictors_url, notice: 'Predictor was successfully destroyed.' }
      format.json { head :no_content }
    end
  end

  def start_train
    TrainJob.perform_later @predictor
    redirect_to predictors_url, notice: 'Train job is submitted.'
  end

  private
    # Use callbacks to share common setup or constraints between actions.
    def set_predictor
      @predictor = Predictor.find(params[:id] || params[:predictor_id])
    end

    # Never trust parameters from the scary internet, only allow the white list through.
    def predictor_params
      params.require(:predictor).permit(:training_set, :train_net, :test_set, :predict_net)
    end
end
