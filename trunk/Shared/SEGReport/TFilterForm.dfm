inherited FilterForm: TFilterForm
  Caption = 'Filter form'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object Label3: TLabel [2]
    Left = 38
    Top = 80
    Width = 25
    Height = 13
    Caption = 'Filter:'
  end
  inherited ToolbarCheckBox: TCheckBox
    TabOrder = 3
  end
  object FilterEdit: TEdit
    Left = 72
    Top = 80
    Width = 145
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    BevelKind = bkSoft
    BorderStyle = bsNone
    TabOrder = 2
    OnExit = FilterEditExit
  end
end
