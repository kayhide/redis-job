require 'test_helper'

class PredictorsControllerTest < ActionDispatch::IntegrationTest
  setup do
    @predictor = predictors(:one)
  end

  test "should get index" do
    get predictors_url
    assert_response :success
  end

  test "should get new" do
    get new_predictor_url
    assert_response :success
  end

  test "should create predictor" do
    assert_difference('Predictor.count') do
      post predictors_url, params: { predictor: { predict_net: @predictor.predict_net, test_set: @predictor.test_set, train_net: @predictor.train_net, training_set: @predictor.training_set } }
    end

    assert_redirected_to predictor_url(Predictor.last)
  end

  test "should show predictor" do
    get predictor_url(@predictor)
    assert_response :success
  end

  test "should get edit" do
    get edit_predictor_url(@predictor)
    assert_response :success
  end

  test "should update predictor" do
    patch predictor_url(@predictor), params: { predictor: { predict_net: @predictor.predict_net, test_set: @predictor.test_set, train_net: @predictor.train_net, training_set: @predictor.training_set } }
    assert_redirected_to predictor_url(@predictor)
  end

  test "should destroy predictor" do
    assert_difference('Predictor.count', -1) do
      delete predictor_url(@predictor)
    end

    assert_redirected_to predictors_url
  end
end
