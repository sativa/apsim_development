inherited Time_series_form: TTime_series_form
  Caption = 'Time series'
  ClientHeight = 699
  PixelsPerInch = 96
  TextHeight = 16
  object Base_dataset_label: TLabel [0]
    Left = 336
    Top = 352
    Width = 83
    Height = 16
    Caption = 'Base dataset:'
  end
  object Percentile_label: TLabel [1]
    Left = 336
    Top = 400
    Width = 63
    Height = 16
    Caption = 'Percentile:'
  end
  object Diff_from_percentile_radio: TRadioButton [6]
    Left = 320
    Top = 328
    Width = 179
    Height = 21
    Caption = 'Difference from percentile'
    TabOrder = 7
    OnClick = Enable_percentile
  end
  object Percentile_edit: TEdit [7]
    Left = 401
    Top = 398
    Width = 31
    Height = 24
    TabOrder = 9
    Text = '50'
  end
  object Base_dataset_combo: TComboBox [8]
    Left = 336
    Top = 368
    Width = 305
    Height = 24
    ItemHeight = 16
    TabOrder = 2
  end
  object Time_series_mean_radio: TRadioButton [9]
    Left = 320
    Top = 256
    Width = 182
    Height = 21
    Caption = 'Time series with mean line'
    TabOrder = 11
    OnClick = Enable_percentile
  end
  object Time_series_percentile_radio: TRadioButton [10]
    Left = 320
    Top = 280
    Width = 214
    Height = 21
    Caption = 'Time series with percentile line'
    TabOrder = 5
    OnClick = Enable_percentile
  end
  object Different_colours_checkbox: TCheckBox [11]
    Left = 336
    Top = 432
    Width = 305
    Height = 25
    Caption = 'Bars above line different colour to below line'
    TabOrder = 4
  end
  object Diff_from_mean_radio: TRadioButton [12]
    Left = 320
    Top = 304
    Width = 179
    Height = 21
    Caption = 'Difference from mean'
    TabOrder = 8
    OnClick = Enable_percentile
  end
  object Time_series_radio: TRadioButton [13]
    Left = 320
    Top = 232
    Width = 139
    Height = 21
    Caption = 'Time series'
    Checked = True
    TabOrder = 10
    TabStop = True
    OnClick = Enable_percentile
  end
  inherited BitBtn2: TBitBtn
    TabOrder = 6
  end
  inherited VariableList: TListView
    Width = 297
    TabOrder = 1
  end
end
