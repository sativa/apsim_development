object RotationForm: TRotationForm
  Left = 247
  Top = 156
  Width = 272
  Height = 145
  BorderIcons = []
  Caption = 'Rotations'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object BitBtn2: TBitBtn
    Left = 128
    Top = 80
    Width = 75
    Height = 25
    TabOrder = 0
    Kind = bkCancel
  end
  object BitBtn1: TBitBtn
    Left = 48
    Top = 80
    Width = 75
    Height = 25
    TabOrder = 1
    Kind = bkOK
  end
  object RotationCheck: TCheckBox
    Left = 16
    Top = 32
    Width = 225
    Height = 17
    Caption = 'Treat all scenarios as a set of rotation files.'
    Checked = True
    State = cbChecked
    TabOrder = 2
  end
end
