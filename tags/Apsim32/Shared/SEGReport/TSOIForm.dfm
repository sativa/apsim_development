inherited SOIForm: TSOIForm
  Caption = 'SOIForm'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object Label3: TLabel [3]
    Left = 17
    Top = 104
    Width = 46
    Height = 13
    Caption = 'Negative:'
  end
  object Label4: TLabel [4]
    Left = 23
    Top = 128
    Width = 40
    Height = 13
    Caption = 'Positive:'
  end
  object Label5: TLabel [5]
    Left = 30
    Top = 152
    Width = 33
    Height = 13
    Caption = 'Falling:'
  end
  object Label6: TLabel [6]
    Left = 31
    Top = 176
    Width = 32
    Height = 13
    Caption = 'Rising:'
  end
  object Label7: TLabel [7]
    Left = 38
    Top = 200
    Width = 25
    Height = 13
    Caption = 'Zero:'
  end
  object Label8: TLabel [8]
    Left = 30
    Top = 80
    Width = 33
    Height = 13
    Caption = 'Month:'
  end
  inherited ToolbarCheckBox: TCheckBox
    TabOrder = 8
  end
  object MonthCombo: TComboBox
    Left = 72
    Top = 80
    Width = 145
    Height = 21
    BevelKind = bkSoft
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 2
    OnClick = MonthComboClick
    Items.Strings = (
      'January'
      'February'
      'March'
      'April'
      'May'
      'June'
      'July'
      'August'
      'September'
      'October'
      'November'
      'December')
  end
  object NegativeCheckBox: TCheckBox
    Left = 72
    Top = 104
    Width = 97
    Height = 17
    TabOrder = 3
    OnClick = NegativeCheckBoxClick
  end
  object PositiveCheckBox: TCheckBox
    Left = 72
    Top = 128
    Width = 97
    Height = 17
    TabOrder = 4
    OnClick = PositiveCheckBoxClick
  end
  object FallingCheckBox: TCheckBox
    Left = 72
    Top = 152
    Width = 97
    Height = 17
    TabOrder = 5
    OnClick = FallingCheckBoxClick
  end
  object RisingCheckBox: TCheckBox
    Left = 72
    Top = 176
    Width = 97
    Height = 17
    TabOrder = 6
    OnClick = RisingCheckBoxClick
  end
  object ZeroCheckBox: TCheckBox
    Left = 72
    Top = 200
    Width = 97
    Height = 17
    TabOrder = 7
    OnClick = ZeroCheckBoxClick
  end
end
