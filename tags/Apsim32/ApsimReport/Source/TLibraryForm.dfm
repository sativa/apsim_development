object LibraryForm: TLibraryForm
  Left = 250
  Top = 114
  Width = 408
  Height = 361
  Caption = 'New Items'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object TabControl: TTabControl
    Left = 0
    Top = 0
    Width = 400
    Height = 286
    Align = alClient
    TabOrder = 0
    OnChange = TabControlChange
    object FileList: TFileListBoxEx
      Left = 4
      Top = 6
      Width = 392
      Height = 276
      Align = alClient
      BevelInner = bvNone
      BevelOuter = bvNone
      ItemHeight = 16
      Mask = '*.report'
      TabOrder = 0
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 286
    Width = 400
    Height = 41
    Align = alBottom
    TabOrder = 1
    DesignSize = (
      400
      41)
    object BitBtn1: TBitBtn
      Left = 232
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      TabOrder = 0
      Kind = bkOK
    end
    object BitBtn2: TBitBtn
      Left = 312
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      TabOrder = 1
      Kind = bkCancel
    end
  end
end
