inherited ValueSelectPopup: TValueSelectPopup
  Left = 320
  Top = 255
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
    Height = 206
    Align = alClient
    BorderStyle = bsNone
    Checkboxes = True
    Columns = <
      item
        AutoSize = True
      end>
    FlatScrollBars = True
    Items.Data = {
      3C0000000200000000000000FFFFFFFFFFFFFFFF00000000000000000568656C
      6C6F00000000FFFFFFFFFFFFFFFF000000000000000005776F726C64}
    ShowColumnHeaders = False
    TabOrder = 0
    ViewStyle = vsReport
    OnChange = ListViewChange
    OnCompare = ListViewCompare
    OnMouseDown = FormMouseDown
  end
  object Panel1: TPanel
    Left = 0
    Top = 206
    Width = 203
    Height = 31
    Align = alBottom
    BevelOuter = bvNone
    Color = clWindow
    TabOrder = 1
    OnMouseDown = FormMouseDown
    object applyLabel: TLabel
      Left = 16
      Top = 8
      Width = 35
      Height = 13
      Cursor = crHandPoint
      Caption = 'Apply...'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGreen
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsUnderline]
      ParentFont = False
      OnClick = applyLabelClick
    end
    object applyToAllLabel: TLabel
      Left = 96
      Top = 8
      Width = 61
      Height = 13
      Cursor = crHandPoint
      Caption = 'Apply to All...'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGreen
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsUnderline]
      ParentFont = False
      OnClick = applyToAllLabelClick
    end
  end
end
