object LibraryForm: TLibraryForm
  Left = 421
  Top = 194
  Width = 593
  Height = 421
  Caption = 'New Items'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  DesignSize = (
    585
    387)
  PixelsPerInch = 120
  TextHeight = 16
  object TabControl: TTabControl
    Left = 0
    Top = 0
    Width = 484
    Height = 382
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    OnChange = TabControlChange
    object FileList: TFileListBoxEx
      Left = 4
      Top = 6
      Width = 476
      Height = 372
      Align = alClient
      BevelEdges = []
      BevelInner = bvNone
      BevelOuter = bvNone
      ItemHeight = 16
      Mask = '*.report'
      ShowGlyphs = True
      TabOrder = 0
    end
  end
  object OkButton: TButton
    Left = 497
    Top = 9
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Ok'
    ModalResult = 1
    TabOrder = 1
  end
  object Button1: TButton
    Left = 497
    Top = 44
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
