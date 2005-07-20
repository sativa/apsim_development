object MissingParametersForm: TMissingParametersForm
  Left = 629
  Top = 186
  Width = 516
  Height = 483
  Caption = 'Missing Parameters'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    508
    449)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 93
    Height = 13
    Caption = 'Missing parameters:'
  end
  object Memo: TMemo
    Left = 16
    Top = 32
    Width = 394
    Height = 410
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'Memo')
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object OkButton: TButton
    Left = 417
    Top = 32
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
end
