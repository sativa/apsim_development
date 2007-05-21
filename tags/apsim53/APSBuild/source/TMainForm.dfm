object MainForm: TMainForm
  Left = 234
  Top = 111
  Anchors = []
  BorderIcons = []
  BorderStyle = bsSingle
  Caption = 'Background compiling'
  ClientHeight = 77
  ClientWidth = 411
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefault
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel3: TPanel
    Left = 0
    Top = 36
    Width = 411
    Height = 41
    Align = alBottom
    TabOrder = 0
    object OnTopRadio: TCheckBox
      Left = 8
      Top = 8
      Width = 89
      Height = 17
      Caption = 'Always on top'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = OnTopRadioClick
    end
    object CloseButton: TButton
      Left = 168
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Cancel'
      ModalResult = 1
      TabOrder = 1
      OnClick = CloseButtonClick
    end
  end
  object ListBox: THTMListBox
    Left = 0
    Top = 0
    Width = 411
    Height = 38
    Align = alTop
    Ellipsis = False
    Enabled = False
    ItemHeight = 16
    Items.Strings = (
      ''
      '')
    Multiline = False
    SelectionColor = clHighlight
    SelectionFontColor = clHighlightText
    ShadowOffset = 1
    Sorted = False
    SortWithHTML = False
    TabOrder = 1
  end
  object Memo: TMemo
    Left = 0
    Top = 38
    Width = 411
    Height = 0
    Align = alClient
    Lines.Strings = (
      'Memo')
    ScrollBars = ssBoth
    TabOrder = 2
    WordWrap = False
  end
end
