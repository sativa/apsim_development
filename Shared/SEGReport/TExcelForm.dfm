inherited ExcelForm: TExcelForm
  Width = 235
  Caption = 'ExcelForm'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object Label3: TLabel [2]
    Left = 18
    Top = 80
    Width = 45
    Height = 13
    Caption = 'Filename:'
  end
  object Label4: TLabel [3]
    Left = 8
    Top = 104
    Width = 55
    Height = 13
    Caption = 'Experiment:'
  end
  inherited SourceCombo: TComboBox
    Width = 143
  end
  inherited NameEdit: TEdit
    Width = 143
  end
  inherited ToolbarCheckBox: TCheckBox
    TabOrder = 4
  end
  object FilenameEdit: TAdvFileNameEdit
    Left = 72
    Top = 80
    Width = 144
    Height = 21
    AutoFocus = False
    ErrorMarkerPos = 0
    ErrorMarkerLen = 0
    ErrorColor = clRed
    ErrorFontColor = clWhite
    Flat = False
    FlatLineColor = clBlack
    FlatParentColor = True
    FocusAlign = eaDefault
    FocusBorder = False
    FocusColor = clWindow
    FocusFontColor = clWindowText
    FocusLabel = False
    FocusWidthInc = 0
    ModifiedColor = clHighlight
    DisabledColor = clSilver
    ReturnIsTab = False
    LengthLimit = 0
    TabOnFullLength = False
    LabelPosition = lpLeftTop
    LabelMargin = 4
    LabelTransparent = False
    LabelAlwaysEnabled = False
    LabelFont.Charset = DEFAULT_CHARSET
    LabelFont.Color = clWindowText
    LabelFont.Height = -11
    LabelFont.Name = 'MS Sans Serif'
    LabelFont.Style = []
    Lookup.CaseSensitive = False
    Lookup.Color = clWindow
    Lookup.DisplayCount = 4
    Lookup.Enabled = False
    Lookup.NumChars = 2
    Persistence.Enable = False
    Persistence.Location = plInifile
    BevelKind = bkSoft
    Anchors = [akLeft, akTop, akRight]
    BorderStyle = bsNone
    Color = clWindow
    Enabled = True
    HintShowLargeText = False
    OleDropTarget = False
    OleDropSource = False
    TabOrder = 2
    Transparent = False
    Visible = True
    OnChange = FilenameEditChange
    ButtonWidth = 18
    Etched = False
    Glyph.Data = {
      CE000000424DCE0000000000000076000000280000000C0000000B0000000100
      0400000000005800000000000000000000001000000000000000000000000000
      8000008000000080800080000000800080008080000080808000C0C0C0000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00D00000000DDD
      00000077777770DD00000F077777770D00000FF07777777000000FFF00000000
      00000FFFFFFF0DDD00000FFF00000DDD0000D000DDDDD0000000DDDDDDDDDD00
      0000DDDDD0DDD0D00000DDDDDD000DDD0000}
    DefaultExt = 'xls'
    Filter = 'Excel files(*.xls)|*.xls'
    FilterIndex = 0
    DialogOptions = []
    DialogKind = fdOpen
  end
  object PageCombo: TComboBox
    Left = 72
    Top = 104
    Width = 144
    Height = 21
    BevelKind = bkSoft
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 3
    OnChange = PageComboChange
  end
end
