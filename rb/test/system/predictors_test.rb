require "application_system_test_case"

class PredictorsTest < ApplicationSystemTestCase
  setup do
    @predictor = predictors(:one)
  end

  test "visiting the index" do
    visit predictors_url
    assert_selector "h1", text: "Predictors"
  end

  test "creating a Predictor" do
    visit predictors_url
    click_on "New Predictor"

    fill_in "Predict net", with: @predictor.predict_net
    fill_in "Test set", with: @predictor.test_set
    fill_in "Train net", with: @predictor.train_net
    fill_in "Training set", with: @predictor.training_set
    click_on "Create Predictor"

    assert_text "Predictor was successfully created"
    click_on "Back"
  end

  test "updating a Predictor" do
    visit predictors_url
    click_on "Edit", match: :first

    fill_in "Predict net", with: @predictor.predict_net
    fill_in "Test set", with: @predictor.test_set
    fill_in "Train net", with: @predictor.train_net
    fill_in "Training set", with: @predictor.training_set
    click_on "Update Predictor"

    assert_text "Predictor was successfully updated"
    click_on "Back"
  end

  test "destroying a Predictor" do
    visit predictors_url
    page.accept_confirm do
      click_on "Destroy", match: :first
    end

    assert_text "Predictor was successfully destroyed"
  end
end
