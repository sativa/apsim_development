inherited StatsForm: TStatsForm
  Height = 470
  Caption = 'StatsForm'
  OldCreateOrder = True
  PixelsPerInch = 120
  TextHeight = 16
  object Label3: TLabel [2]
    Left = 11
    Top = 130
    Width = 70
    Height = 16
    Caption = 'Field name:'
  end
  object Label4: TLabel [4]
    Left = 23
    Top = 172
    Width = 56
    Height = 16
    Caption = 'Minimum:'
  end
  object Label5: TLabel [5]
    Left = 20
    Top = 191
    Width = 60
    Height = 16
    Caption = 'Maximum:'
  end
  object Label6: TLabel [6]
    Left = 39
    Top = 211
    Width = 37
    Height = 16
    Caption = 'Count:'
  end
  object Label7: TLabel [7]
    Left = 49
    Top = 250
    Width = 29
    Height = 16
    Caption = '10%:'
  end
  object Label8: TLabel [8]
    Left = 49
    Top = 270
    Width = 29
    Height = 16
    Caption = '20%:'
  end
  object Label9: TLabel [9]
    Left = 49
    Top = 290
    Width = 29
    Height = 16
    Caption = '30%:'
  end
  object Label10: TLabel [10]
    Left = 49
    Top = 310
    Width = 29
    Height = 16
    Caption = '40%:'
  end
  object Label11: TLabel [11]
    Left = 49
    Top = 329
    Width = 29
    Height = 16
    Caption = '50%:'
  end
  object Label12: TLabel [12]
    Left = 49
    Top = 349
    Width = 29
    Height = 16
    Caption = '60%:'
  end
  object Label13: TLabel [13]
    Left = 49
    Top = 369
    Width = 29
    Height = 16
    Caption = '70%:'
  end
  object Label14: TLabel [14]
    Left = 49
    Top = 388
    Width = 29
    Height = 16
    Caption = '80%:'
  end
  object Label15: TLabel [15]
    Left = 49
    Top = 408
    Width = 29
    Height = 16
    Caption = '90%:'
  end
  object Label16: TLabel [16]
    Left = 41
    Top = 152
    Width = 37
    Height = 16
    Caption = 'Mean:'
  end
  object Label17: TLabel [18]
    Left = 47
    Top = 230
    Width = 30
    Height = 16
    Caption = 'Sum:'
  end
  object FieldNameCombo: TComboBox
    Left = 89
    Top = 126
    Width = 178
    Height = 24
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 16
    TabOrder = 4
    OnChange = FieldNameComboChange
  end
  object MeanCheckBox: TCheckBox
    Left = 89
    Top = 152
    Width = 119
    Height = 21
    TabOrder = 5
    OnClick = CheckBoxClick
  end
  object MinCheckBox: TCheckBox
    Left = 89
    Top = 172
    Width = 119
    Height = 21
    TabOrder = 6
    OnClick = CheckBoxClick
  end
  object MaxCheckBox: TCheckBox
    Left = 89
    Top = 191
    Width = 119
    Height = 21
    TabOrder = 7
    OnClick = CheckBoxClick
  end
  object CountCheckBox: TCheckBox
    Left = 89
    Top = 211
    Width = 119
    Height = 21
    TabOrder = 8
    OnClick = CheckBoxClick
  end
  object Decile10CheckBox: TCheckBox
    Left = 89
    Top = 250
    Width = 70
    Height = 21
    TabOrder = 10
    OnClick = CheckBoxClick
  end
  object Decile20CheckBox: TCheckBox
    Left = 89
    Top = 270
    Width = 70
    Height = 21
    TabOrder = 11
    OnClick = CheckBoxClick
  end
  object Decile30CheckBox: TCheckBox
    Left = 89
    Top = 290
    Width = 70
    Height = 21
    TabOrder = 12
    OnClick = CheckBoxClick
  end
  object Decile40CheckBox: TCheckBox
    Left = 89
    Top = 310
    Width = 70
    Height = 20
    TabOrder = 13
    OnClick = CheckBoxClick
  end
  object Decile50CheckBox: TCheckBox
    Left = 89
    Top = 329
    Width = 60
    Height = 21
    TabOrder = 14
    OnClick = CheckBoxClick
  end
  object Decile60CheckBox: TCheckBox
    Left = 89
    Top = 349
    Width = 70
    Height = 21
    TabOrder = 15
    OnClick = CheckBoxClick
  end
  object Decile70CheckBox: TCheckBox
    Left = 89
    Top = 369
    Width = 70
    Height = 21
    TabOrder = 16
    OnClick = CheckBoxClick
  end
  object Decile80CheckBox: TCheckBox
    Left = 89
    Top = 388
    Width = 70
    Height = 21
    TabOrder = 17
    OnClick = CheckBoxClick
  end
  object Decile90CheckBox: TCheckBox
    Left = 89
    Top = 408
    Width = 70
    Height = 21
    TabOrder = 18
    OnClick = CheckBoxClick
  end
  object SumCheckBox: TCheckBox
    Left = 89
    Top = 231
    Width = 119
    Height = 21
    TabOrder = 9
    OnClick = CheckBoxClick
  end
end
