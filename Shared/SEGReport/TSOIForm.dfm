inherited SOIForm: TSOIForm
  Caption = 'SOIForm'
  OldCreateOrder = True
  PixelsPerInch = 120
  TextHeight = 16
  object Label3: TLabel [3]
    Left = 21
    Top = 192
    Width = 58
    Height = 16
    Caption = 'Negative:'
  end
  object Label4: TLabel [4]
    Left = 28
    Top = 222
    Width = 51
    Height = 16
    Caption = 'Positive:'
  end
  object Label5: TLabel [5]
    Left = 37
    Top = 251
    Width = 43
    Height = 16
    Caption = 'Falling:'
  end
  object Label6: TLabel [6]
    Left = 38
    Top = 281
    Width = 41
    Height = 16
    Caption = 'Rising:'
  end
  object Label7: TLabel [7]
    Left = 47
    Top = 310
    Width = 31
    Height = 16
    Caption = 'Zero:'
  end
  object Label8: TLabel [8]
    Left = 37
    Top = 164
    Width = 39
    Height = 16
    Caption = 'Month:'
  end
  inherited GroupByEdit: TEdit
    TabOrder = 10
  end
  object MonthCombo: TComboBox
    Left = 89
    Top = 162
    Width = 178
    Height = 24
    BevelKind = bkSoft
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 16
    TabOrder = 4
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
    Left = 89
    Top = 192
    Width = 119
    Height = 21
    TabOrder = 5
    OnClick = NegativeCheckBoxClick
  end
  object PositiveCheckBox: TCheckBox
    Left = 89
    Top = 222
    Width = 119
    Height = 20
    TabOrder = 6
    OnClick = PositiveCheckBoxClick
  end
  object FallingCheckBox: TCheckBox
    Left = 89
    Top = 251
    Width = 119
    Height = 21
    TabOrder = 7
    OnClick = FallingCheckBoxClick
  end
  object RisingCheckBox: TCheckBox
    Left = 89
    Top = 281
    Width = 119
    Height = 21
    TabOrder = 8
    OnClick = RisingCheckBoxClick
  end
  object ZeroCheckBox: TCheckBox
    Left = 89
    Top = 310
    Width = 119
    Height = 21
    TabOrder = 9
    OnClick = ZeroCheckBoxClick
  end
end
