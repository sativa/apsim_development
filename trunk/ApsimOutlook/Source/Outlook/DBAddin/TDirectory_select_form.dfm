object Directory_select_form: TDirectory_select_form
  Left = 391
  Top = 206
  AutoScroll = False
  Caption = 'Dataset selection form'
  ClientHeight = 435
  ClientWidth = 378
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 16
  object Label1: TLabel
    Left = 24
    Top = 8
    Width = 90
    Height = 16
    Caption = 'Dataset Name:'
  end
  object Label2: TLabel
    Left = 24
    Top = 64
    Width = 116
    Height = 16
    Caption = 'Subsets of Dataset:'
  end
  object SubsetList: TFileListBox
    Left = 24
    Top = 80
    Width = 329
    Height = 277
    FileType = [ftDirectory]
    ItemHeight = 16
    Mask = '*'
    MultiSelect = True
    ShowGlyphs = True
    TabOrder = 0
    OnChange = SubsetListChange
    OnClick = SubsetListChange
  end
  object Ok_button: TBitBtn
    Left = 89
    Top = 384
    Width = 92
    Height = 31
    TabOrder = 1
    Kind = bkOK
  end
  object BitBtn2: TBitBtn
    Left = 197
    Top = 384
    Width = 92
    Height = 31
    TabOrder = 2
    Kind = bkCancel
  end
  object DatasetCombo: TComboBox
    Left = 24
    Top = 24
    Width = 329
    Height = 24
    ItemHeight = 16
    TabOrder = 3
    OnChange = DatasetComboChange
  end
end
