inherited StatsForm: TStatsForm
  Height = 557
  Caption = 'StatsForm'
  OldCreateOrder = True
  PixelsPerInch = 120
  TextHeight = 16
  object Label3: TLabel [2]
    Left = 11
    Top = 178
    Width = 70
    Height = 16
    Caption = 'Field name:'
  end
  object Label4: TLabel [4]
    Left = 23
    Top = 220
    Width = 56
    Height = 16
    Caption = 'Minimum:'
  end
  object Label5: TLabel [5]
    Left = 20
    Top = 239
    Width = 60
    Height = 16
    Caption = 'Maximum:'
  end
  object Label6: TLabel [6]
    Left = 39
    Top = 259
    Width = 37
    Height = 16
    Caption = 'Count:'
  end
  object Label7: TLabel [7]
    Left = 49
    Top = 298
    Width = 29
    Height = 16
    Caption = '10%:'
  end
  object Label8: TLabel [8]
    Left = 49
    Top = 318
    Width = 29
    Height = 16
    Caption = '20%:'
  end
  object Label9: TLabel [9]
    Left = 49
    Top = 338
    Width = 29
    Height = 16
    Caption = '30%:'
  end
  object Label10: TLabel [10]
    Left = 49
    Top = 358
    Width = 29
    Height = 16
    Caption = '40%:'
  end
  object Label11: TLabel [11]
    Left = 49
    Top = 377
    Width = 29
    Height = 16
    Caption = '50%:'
  end
  object Label12: TLabel [12]
    Left = 49
    Top = 397
    Width = 29
    Height = 16
    Caption = '60%:'
  end
  object Label13: TLabel [13]
    Left = 49
    Top = 417
    Width = 29
    Height = 16
    Caption = '70%:'
  end
  object Label14: TLabel [14]
    Left = 49
    Top = 436
    Width = 29
    Height = 16
    Caption = '80%:'
  end
  object Label15: TLabel [15]
    Left = 49
    Top = 456
    Width = 29
    Height = 16
    Caption = '90%:'
  end
  object Label16: TLabel [16]
    Left = 41
    Top = 200
    Width = 37
    Height = 16
    Caption = 'Mean:'
  end
  object Label17: TLabel [18]
    Left = 47
    Top = 278
    Width = 30
    Height = 16
    Caption = 'Sum:'
  end
  object FieldNameCombo: TComboBox [24]
    Left = 89
    Top = 174
    Width = 178
    Height = 24
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 16
    TabOrder = 19
    OnChange = FieldNameComboChange
  end
  object MeanCheckBox: TCheckBox [25]
    Left = 89
    Top = 200
    Width = 119
    Height = 21
    TabOrder = 5
    OnClick = CheckBoxClick
  end
  object MinCheckBox: TCheckBox [26]
    Left = 89
    Top = 220
    Width = 119
    Height = 21
    TabOrder = 6
    OnClick = CheckBoxClick
  end
  object MaxCheckBox: TCheckBox [27]
    Left = 89
    Top = 239
    Width = 119
    Height = 21
    TabOrder = 7
    OnClick = CheckBoxClick
  end
  object CountCheckBox: TCheckBox [28]
    Left = 89
    Top = 259
    Width = 119
    Height = 21
    TabOrder = 8
    OnClick = CheckBoxClick
  end
  object Decile10CheckBox: TCheckBox [29]
    Left = 89
    Top = 298
    Width = 70
    Height = 21
    TabOrder = 10
    OnClick = CheckBoxClick
  end
  object Decile20CheckBox: TCheckBox [30]
    Left = 89
    Top = 318
    Width = 70
    Height = 21
    TabOrder = 11
    OnClick = CheckBoxClick
  end
  object Decile30CheckBox: TCheckBox [31]
    Left = 89
    Top = 338
    Width = 70
    Height = 21
    TabOrder = 12
    OnClick = CheckBoxClick
  end
  object Decile40CheckBox: TCheckBox [32]
    Left = 89
    Top = 358
    Width = 70
    Height = 20
    TabOrder = 13
    OnClick = CheckBoxClick
  end
  object Decile50CheckBox: TCheckBox [33]
    Left = 89
    Top = 377
    Width = 60
    Height = 21
    TabOrder = 14
    OnClick = CheckBoxClick
  end
  object Decile60CheckBox: TCheckBox [34]
    Left = 89
    Top = 397
    Width = 70
    Height = 21
    TabOrder = 15
    OnClick = CheckBoxClick
  end
  object Decile70CheckBox: TCheckBox [35]
    Left = 89
    Top = 417
    Width = 70
    Height = 21
    TabOrder = 16
    OnClick = CheckBoxClick
  end
  object Decile80CheckBox: TCheckBox [36]
    Left = 89
    Top = 436
    Width = 70
    Height = 21
    TabOrder = 17
    OnClick = CheckBoxClick
  end
  object Decile90CheckBox: TCheckBox [37]
    Left = 89
    Top = 456
    Width = 70
    Height = 21
    TabOrder = 18
    OnClick = CheckBoxClick
  end
  object SumCheckBox: TCheckBox [38]
    Left = 89
    Top = 279
    Width = 119
    Height = 21
    TabOrder = 9
    OnClick = CheckBoxClick
  end
end
