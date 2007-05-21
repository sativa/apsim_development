inherited Probability_analysis_form: TProbability_analysis_form
  Caption = 'Probability'
  PixelsPerInch = 96
  TextHeight = 16
  inherited BitBtn2: TBitBtn
    TabOrder = 4
  end
  inherited VariableList: TListView
    Width = 297
    TabOrder = 5
  end
  object Probability_exceedence_radio: TRadioButton [8]
    Left = 331
    Top = 292
    Width = 179
    Height = 21
    Caption = 'Probability exceedence'
    TabOrder = 2
  end
  object Cumulative_probability_radio: TRadioButton [9]
    Left = 331
    Top = 322
    Width = 179
    Height = 21
    Caption = 'Cumulative probability'
    Checked = True
    TabOrder = 1
    TabStop = True
  end
end
