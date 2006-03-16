object CropForm: TCropForm
  Left = 400
  Top = 214
  BorderStyle = bsDialog
  Caption = 'Select Crops'
  ClientHeight = 267
  ClientWidth = 242
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 8
    Width = 159
    Height = 26
    Caption = 'Select the crops you want to add to your economic scenario'
    WordWrap = True
  end
  object CropList: TCheckListBox
    Left = 15
    Top = 39
    Width = 215
    Height = 182
    BevelKind = bkFlat
    BorderStyle = bsNone
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ItemHeight = 16
    ParentFont = False
    TabOrder = 0
  end
  object Button1: TButton
    Left = 74
    Top = 232
    Width = 75
    Height = 25
    Caption = '&Ok'
    ModalResult = 1
    TabOrder = 1
  end
  object Button2: TButton
    Left = 154
    Top = 232
    Width = 75
    Height = 25
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
