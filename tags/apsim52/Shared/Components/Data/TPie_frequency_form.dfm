inherited Pie_frequency_form: TPie_frequency_form
  Caption = 'Pie frequency form'
  PixelsPerInch = 96
  TextHeight = 16
  object Label3: TLabel [1]
    Left = 320
    Top = 347
    Width = 116
    Height = 16
    Caption = 'Number of wedges:'
  end
  object Label4: TLabel [2]
    Left = 369
    Top = 375
    Width = 70
    Height = 16
    Caption = 'Percentiles:'
  end
  object Label5: TLabel [4]
    Left = 288
    Top = 268
    Width = 298
    Height = 16
    Caption = 'Compare the base dataset to which other analysis.'
  end
  object Label6: TLabel [5]
    Left = 288
    Top = 220
    Width = 134
    Height = 16
    Caption = 'Select a base dataset:'
  end
  inherited BitBtn2: TBitBtn
    TabOrder = 6
  end
  inherited VariableList: TListView
    TabOrder = 5
  end
  inherited ScenarioTree: TTreeView
    TabOrder = 7
  end
  object Num_percentiles_edit: TEdit [12]
    Left = 440
    Top = 344
    Width = 73
    Height = 24
    TabOrder = 2
    Text = '3'
    OnChange = Num_percentiles_editChange
  end
  object Percentile_grid: TStringGrid [13]
    Left = 440
    Top = 376
    Width = 71
    Height = 106
    ColCount = 1
    DefaultRowHeight = 16
    FixedCols = 0
    RowCount = 6
    FixedRows = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
    ScrollBars = ssNone
    TabOrder = 1
  end
  object BaseDataset: TComboBox [14]
    Left = 288
    Top = 240
    Width = 329
    Height = 24
    ItemHeight = 16
    TabOrder = 3
  end
  object SecondDataset: TComboBox [15]
    Left = 288
    Top = 288
    Width = 329
    Height = 24
    ItemHeight = 16
    TabOrder = 4
  end
end
