inherited StatsForm: TStatsForm
  Caption = 'StatsForm'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object Label3: TLabel [2]
    Left = 9
    Top = 80
    Width = 54
    Height = 13
    Caption = 'Field name:'
  end
  object Label4: TLabel [4]
    Left = 19
    Top = 120
    Width = 44
    Height = 13
    Caption = 'Minimum:'
  end
  object Label5: TLabel [5]
    Left = 16
    Top = 136
    Width = 47
    Height = 13
    Caption = 'Maximum:'
  end
  object Label6: TLabel [6]
    Left = 32
    Top = 152
    Width = 31
    Height = 13
    Caption = 'Count:'
  end
  object Label7: TLabel [7]
    Left = 40
    Top = 184
    Width = 23
    Height = 13
    Caption = '10%:'
  end
  object Label8: TLabel [8]
    Left = 40
    Top = 200
    Width = 23
    Height = 13
    Caption = '20%:'
  end
  object Label9: TLabel [9]
    Left = 40
    Top = 216
    Width = 23
    Height = 13
    Caption = '30%:'
  end
  object Label10: TLabel [10]
    Left = 40
    Top = 232
    Width = 23
    Height = 13
    Caption = '40%:'
  end
  object Label11: TLabel [11]
    Left = 40
    Top = 248
    Width = 23
    Height = 13
    Caption = '50%:'
  end
  object Label12: TLabel [12]
    Left = 40
    Top = 264
    Width = 23
    Height = 13
    Caption = '60%:'
  end
  object Label13: TLabel [13]
    Left = 40
    Top = 280
    Width = 23
    Height = 13
    Caption = '70%:'
  end
  object Label14: TLabel [14]
    Left = 40
    Top = 296
    Width = 23
    Height = 13
    Caption = '80%:'
  end
  object Label15: TLabel [15]
    Left = 40
    Top = 312
    Width = 23
    Height = 13
    Caption = '90%:'
  end
  object Label16: TLabel [16]
    Left = 33
    Top = 104
    Width = 30
    Height = 13
    Caption = 'Mean:'
  end
  object Label1: TLabel [17]
    Left = 39
    Top = 168
    Width = 24
    Height = 13
    Caption = 'Sum:'
  end
  inherited ToolbarCheckBox: TCheckBox
    TabOrder = 16
  end
  object FieldNameCombo: TComboBox
    Left = 72
    Top = 80
    Width = 145
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 2
    OnChange = FieldNameComboChange
  end
  object MeanCheckBox: TCheckBox
    Left = 72
    Top = 104
    Width = 97
    Height = 17
    TabOrder = 3
    OnClick = CheckBoxClick
  end
  object MinCheckBox: TCheckBox
    Left = 72
    Top = 120
    Width = 97
    Height = 17
    TabOrder = 4
    OnClick = CheckBoxClick
  end
  object MaxCheckBox: TCheckBox
    Left = 72
    Top = 136
    Width = 97
    Height = 17
    TabOrder = 5
    OnClick = CheckBoxClick
  end
  object CountCheckBox: TCheckBox
    Left = 72
    Top = 152
    Width = 97
    Height = 17
    TabOrder = 6
    OnClick = CheckBoxClick
  end
  object Decile10CheckBox: TCheckBox
    Left = 72
    Top = 184
    Width = 57
    Height = 17
    TabOrder = 7
    OnClick = CheckBoxClick
  end
  object Decile20CheckBox: TCheckBox
    Left = 72
    Top = 200
    Width = 57
    Height = 17
    TabOrder = 8
    OnClick = CheckBoxClick
  end
  object Decile30CheckBox: TCheckBox
    Left = 72
    Top = 216
    Width = 57
    Height = 17
    TabOrder = 9
    OnClick = CheckBoxClick
  end
  object Decile40CheckBox: TCheckBox
    Left = 72
    Top = 232
    Width = 57
    Height = 17
    TabOrder = 10
    OnClick = CheckBoxClick
  end
  object Decile50CheckBox: TCheckBox
    Left = 72
    Top = 248
    Width = 49
    Height = 17
    TabOrder = 11
    OnClick = CheckBoxClick
  end
  object Decile60CheckBox: TCheckBox
    Left = 72
    Top = 264
    Width = 57
    Height = 17
    TabOrder = 12
    OnClick = CheckBoxClick
  end
  object Decile70CheckBox: TCheckBox
    Left = 72
    Top = 280
    Width = 57
    Height = 17
    TabOrder = 13
    OnClick = CheckBoxClick
  end
  object Decile80CheckBox: TCheckBox
    Left = 72
    Top = 296
    Width = 57
    Height = 17
    TabOrder = 14
    OnClick = CheckBoxClick
  end
  object Decile90CheckBox: TCheckBox
    Left = 72
    Top = 312
    Width = 57
    Height = 17
    TabOrder = 15
    OnClick = CheckBoxClick
  end
  object SumCheckBox: TCheckBox
    Left = 72
    Top = 168
    Width = 97
    Height = 17
    TabOrder = 17
    OnClick = CheckBoxClick
  end
end
