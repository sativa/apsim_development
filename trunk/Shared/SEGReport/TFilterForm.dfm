inherited FilterForm: TFilterForm
  Caption = 'Filter form'
  OldCreateOrder = True
  PixelsPerInch = 120
  TextHeight = 16
  object FilterLabel: TLabel [2]
    Left = 47
    Top = 166
    Width = 32
    Height = 16
    Caption = 'Filter:'
  end
  inherited GroupByEdit: TEdit
    TabOrder = 5
  end
  object FilterEdit: TEdit
    Left = 89
    Top = 164
    Width = 178
    Height = 26
    Anchors = [akLeft, akTop, akRight]
    BevelKind = bkSoft
    BorderStyle = bsNone
    TabOrder = 4
    OnExit = FilterEditExit
  end
end
