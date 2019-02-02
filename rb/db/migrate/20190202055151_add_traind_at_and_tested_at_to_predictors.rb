class AddTraindAtAndTestedAtToPredictors < ActiveRecord::Migration[5.2]
  def change
    add_column :predictors, :trained_at, :datetime
    add_column :predictors, :tested_at, :datetime
  end
end
