object PropertyForm: TPropertyForm
  Left = 442
  Top = 110
  Width = 279
  Height = 442
  Caption = 'PropertyForm'
  Color = clWindow
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    271
    409)
  PixelsPerInch = 120
  TextHeight = 16
  object SourceLabel: TLabel
    Left = 32
    Top = 39
    Width = 46
    Height = 16
    Caption = 'Source:'
  end
  object Label2: TLabel
    Left = 39
    Top = 10
    Width = 40
    Height = 16
    Caption = 'Name:'
  end
  object ToolbarLabel: TLabel
    Left = 10
    Top = 69
    Width = 69
    Height = 16
    Caption = 'On toolbar?'
  end
  object SortFieldsLabel: TLabel
    Left = 15
    Top = 98
    Width = 62
    Height = 16
    Caption = 'Sort fields:'
  end
  object SourceCombo: TComboBox
    Left = 89
    Top = 39
    Width = 177
    Height = 24
    BevelKind = bkSoft
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 16
    TabOrder = 1
    OnChange = SourceComboChange
  end
  object NameEdit: TEdit
    Left = 89
    Top = 10
    Width = 177
    Height = 26
    Anchors = [akLeft, akTop, akRight]
    BevelKind = bkSoft
    BorderStyle = bsNone
    TabOrder = 0
    OnExit = NameEditExit
  end
  object ToolbarCheckBox: TCheckBox
    Left = 89
    Top = 69
    Width = 119
    Height = 21
    Ctl3D = True
    ParentCtl3D = False
    TabOrder = 2
    OnClick = ToolbarCheckBoxClick
  end
  object SortFieldsEdit: TEdit
    Left = 89
    Top = 98
    Width = 177
    Height = 26
    Anchors = [akLeft, akTop, akRight]
    BevelKind = bkSoft
    BorderStyle = bsNone
    TabOrder = 3
    OnExit = SortFieldsEditChange
  end
end