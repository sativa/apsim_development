inherited ProbabilityForm: TProbabilityForm
  Caption = 'ProbabilityForm'
  OldCreateOrder = True
  PixelsPerInch = 120
  TextHeight = 16
  object Label3: TLabel [2]
    Left = 11
    Top = 131
    Width = 70
    Height = 16
    Caption = 'Field name:'
  end
  object Label4: TLabel [4]
    Left = 22
    Top = 160
    Width = 56
    Height = 16
    Caption = 'Exceed?:'
  end
  inherited SourceCombo: TComboBox
    Width = 174
  end
  inherited NameEdit: TEdit
    Width = 174
  end
  object FieldNameCombo: TComboBox
    Left = 89
    Top = 130
    Width = 174
    Height = 24
    BevelKind = bkSoft
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 16
    TabOrder = 4
    OnChange = FieldNameComboChange
  end
  object ExceedenceCheckBox: TCheckBox
    Left = 89
    Top = 160
    Width = 119
    Height = 21
    TabOrder = 5
    OnClick = ExceedenceCheckBoxClick
  end
end
