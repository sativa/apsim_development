object MoveParametersForm: TMoveParametersForm
  Left = 248
  Top = 234
  AutoScroll = False
  Caption = 'Control File Converter'
  ClientHeight = 199
  ClientWidth = 368
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  DesignSize = (
    368
    199)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 8
    Width = 329
    Height = 97
    Alignment = taCenter
    Caption = 
      'APSIM 3 no longer supports parameters being in  the control file' +
      '. The APSIM control file converter will move them out  automatic' +
      'ally to another file.'
    WordWrap = True
  end
  object Label3: TLabel
    Left = 16
    Top = 120
    Width = 334
    Height = 13
    Caption = 
      'Please enter the name of a file where the parameters will be app' +
      'ended:'
  end
  object FileEdit: TAdvFileNameEdit
    Left = 16
    Top = 136
    Width = 337
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
    Color = clWindow
    Enabled = True
    HintShowLargeText = False
    OleDropTarget = False
    OleDropSource = False
    TabOrder = 0
    Transparent = False
    Visible = True
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
    FilterIndex = 0
    DialogOptions = []
    DialogKind = fdOpen
  end
  object Button1: TButton
    Left = 199
    Top = 168
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Ok'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object Button2: TButton
    Left = 282
    Top = 168
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
