class CreatePredictors < ActiveRecord::Migration[5.2]
  def change
    create_table :predictors do |t|
      t.text :training_set
      t.text :train_net
      t.text :test_set
      t.text :predict_net

      t.timestamps
    end
  end
end
