object MainForm: TMainForm
  Left = 1603
  Top = 681
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Economics conversion'
  ClientHeight = 217
  ClientWidth = 402
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
  object Label1: TLabel
    Left = 0
    Top = 24
    Width = 401
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = 
      'This version of APSIM Outlook uses a different file format for i' +
      't'#39's economics files. '
    WordWrap = True
  end
  object PromptLabel: TLabel
    Left = 0
    Top = 88
    Width = 401
    Height = 57
    Alignment = taCenter
    AutoSize = False
    WordWrap = True
  end
  object OkButton: TButton
    Left = 152
    Top = 168
    Width = 75
    Height = 25
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 0
    OnClick = OkButtonClick
  end
end
