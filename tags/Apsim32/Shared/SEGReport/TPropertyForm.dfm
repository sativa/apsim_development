object PropertyForm: TPropertyForm
  Left = 442
  Top = 110
  Width = 236
  Height = 442
  Caption = 'PropertyForm'
  Color = clWindow
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    228
    408)
  PixelsPerInch = 96
  TextHeight = 13
  object SourceLabel: TLabel
    Left = 26
    Top = 32
    Width = 37
    Height = 13
    Caption = 'Source:'
  end
  object Label2: TLabel
    Left = 32
    Top = 8
    Width = 31
    Height = 13
    Caption = 'Name:'
  end
  object ToolbarLabel: TLabel
    Left = 8
    Top = 56
    Width = 55
    Height = 13
    Caption = 'On toolbar?'
  end
  object SourceCombo: TComboBox
    Left = 72
    Top = 32
    Width = 144
    Height = 21
    BevelKind = bkSoft
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 0
    OnChange = SourceComboChange
  end
  object NameEdit: TEdit
    Left = 72
    Top = 8
    Width = 144
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    BevelKind = bkSoft
    BorderStyle = bsNone
    TabOrder = 1
    OnExit = NameEditExit
  end
  object ToolbarCheckBox: TCheckBox
    Left = 72
    Top = 56
    Width = 97
    Height = 17
    Ctl3D = True
    ParentCtl3D = False
    TabOrder = 2
    OnClick = ToolbarCheckBoxClick
  end
end
