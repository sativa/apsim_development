inherited ProbabilityForm: TProbabilityForm
  Width = 233
  Caption = 'ProbabilityForm'
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
    Left = 18
    Top = 104
    Width = 45
    Height = 13
    Caption = 'Exceed?:'
  end
  inherited SourceCombo: TComboBox
    Width = 141
  end
  inherited NameEdit: TEdit
    Width = 141
  end
  inherited ToolbarCheckBox: TCheckBox
    TabOrder = 4
  end
  object FieldNameCombo: TComboBox
    Left = 72
    Top = 80
    Width = 142
    Height = 21
    BevelKind = bkSoft
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 2
    OnChange = FieldNameComboChange
  end
  object ExceedenceCheckBox: TCheckBox
    Left = 72
    Top = 104
    Width = 97
    Height = 17
    TabOrder = 3
    OnClick = ExceedenceCheckBoxClick
  end
end
