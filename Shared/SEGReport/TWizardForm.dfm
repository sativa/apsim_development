object WizardForm: TWizardForm
  Left = 416
  Top = 148
  Width = 709
  Height = 605
  Caption = 'ApsimReport Wizard'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 16
  object WizardPanel: TPanel
    Left = 160
    Top = 8
    Width = 537
    Height = 521
    TabOrder = 0
  end
  object NextButton: TButton
    Left = 512
    Top = 536
    Width = 75
    Height = 25
    Caption = 'Next >'
    TabOrder = 1
    OnClick = NextButtonClick
  end
  object BackButton: TButton
    Left = 432
    Top = 536
    Width = 75
    Height = 25
    Caption = '< Back'
    Enabled = False
    TabOrder = 2
    OnClick = BackButtonClick
  end
  object CancelButton: TButton
    Left = 616
    Top = 536
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
end
