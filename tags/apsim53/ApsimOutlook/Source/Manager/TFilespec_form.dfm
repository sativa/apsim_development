object Filespec_import_form: TFilespec_import_form
  Left = 361
  Top = 277
  AutoScroll = False
  Caption = 'Filespec import'
  ClientHeight = 177
  ClientWidth = 457
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 20
    Top = 30
    Width = 45
    Height = 13
    Caption = 'Directory:'
  end
  object Label2: TLabel
    Left = 20
    Top = 79
    Width = 42
    Height = 13
    Caption = 'Filespec:'
  end
  object FilespecEdit: TEdit
    Left = 89
    Top = 79
    Width = 64
    Height = 21
    TabOrder = 0
    Text = '*.*'
  end
  object BitBtn1: TBitBtn
    Left = 128
    Top = 128
    Width = 92
    Height = 31
    TabOrder = 2
    Kind = bkOK
  end
  object BitBtn2: TBitBtn
    Left = 236
    Top = 128
    Width = 93
    Height = 31
    TabOrder = 1
    Kind = bkCancel
  end
  object DirectoryEdit: TAdvDirectoryEdit
    Left = 88
    Top = 32
    Width = 345
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
    TabOrder = 3
    Transparent = False
    Visible = True
    ButtonWidth = 18
    Etched = False
    Glyph.Data = {
      CE000000424DCE0000000000000076000000280000000C0000000B0000000100
      0400000000005800000000000000000000001000000000000000000000000000
      8000008000000080800080000000800080008080000080808000C0C0C0000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00F00000000FFF
      00000088888880FF00000B088888880F00000BB08888888000000BBB00000000
      00000BBBBBBB0B0F00000BBB00000B0F0000F000BBBBBB0F0000FF0BBBBBBB0F
      0000FF0BBB00000F0000FFF000FFFFFF0000}
    BrowseDialogText = 'Select Directory'
  end
end
