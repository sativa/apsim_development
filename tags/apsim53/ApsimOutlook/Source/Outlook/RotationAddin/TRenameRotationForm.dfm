object RenameRotationForm: TRenameRotationForm
  Left = 250
  Top = 114
  Width = 264
  Height = 146
  Caption = 'Change rotation name'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 16
    Width = 133
    Height = 13
    Caption = 'Enter new name for rotation:'
  end
  object RotationNameEdit: TEdit
    Left = 8
    Top = 32
    Width = 239
    Height = 25
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object BitBtn1: TBitBtn
    Left = 48
    Top = 72
    Width = 75
    Height = 25
    TabOrder = 1
    Kind = bkOK
  end
  object BitBtn2: TBitBtn
    Left = 128
    Top = 72
    Width = 75
    Height = 25
    TabOrder = 2
    Kind = bkCancel
  end
end
