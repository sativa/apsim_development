inherited ValueSelectPopup: TValueSelectPopup
  Left = 322
  Top = 256
  Caption = 'ValueSelectPopup'
  ClientHeight = 237
  ClientWidth = 203
  OldCreateOrder = True
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ListView: TListView
    Left = 0
    Top = 0
    Width = 203
    Height = 184
    Align = alClient
    BorderStyle = bsNone
    Checkboxes = True
    Columns = <
      item
        AutoSize = True
        Caption = 'A caption'
      end>
    ColumnClick = False
    FlatScrollBars = True
    Items.Data = {
      3C0000000200000000000000FFFFFFFFFFFFFFFF00000000000000000568656C
      6C6F00000000FFFFFFFFFFFFFFFF000000000000000005776F726C64}
    TabOrder = 0
    ViewStyle = vsReport
    OnChange = ListViewChange
    OnCompare = ListViewCompare
    OnMouseDown = FormMouseDown
  end
  object Panel1: TPanel
    Left = 0
    Top = 184
    Width = 203
    Height = 53
    Align = alBottom
    BevelOuter = bvNone
    Color = clWindow
    TabOrder = 1
    OnMouseDown = FormMouseDown
    object applyLabel: TLabel
      Left = 8
      Top = 8
      Width = 126
      Height = 13
      Cursor = crHandPoint
      Caption = 'Apply to current scenario...'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGreen
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsUnderline]
      ParentFont = False
      OnClick = applyLabelClick
    end
    object applyToAllLabel: TLabel
      Left = 8
      Top = 24
      Width = 108
      Height = 13
      Cursor = crHandPoint
      Caption = 'Apply to all scenarios...'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGreen
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsUnderline]
      ParentFont = False
      OnClick = applyToAllLabelClick
    end
    object Image: TImage
      Left = 144
      Top = 2
      Width = 50
      Height = 50
      Transparent = True
    end
  end
end
