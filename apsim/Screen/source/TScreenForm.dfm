object ScreenForm: TScreenForm
  Left = 66
  Top = 116
  BorderStyle = bsSingle
  Caption = 'APSIM'
  ClientHeight = 119
  ClientWidth = 464
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
  object StartDateLabel: TLabel
    Left = 16
    Top = 64
    Width = 37
    Height = 13
  end
  object EndDateLabel: TLabel
    Left = 416
    Top = 64
    Width = 32
    Height = 13
    Alignment = taRightJustify
  end
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 94
    Height = 13
    Caption = 'Simulation progress.'
  end
  object ProgressBar: TProgressBar
    Left = 16
    Top = 40
    Width = 433
    Height = 17
    Min = 0
    Max = 100
    TabOrder = 0
  end
  object Memo: TMemo
    Left = 16
    Top = 80
    Width = 433
    Height = 289
    BevelInner = bvNone
    BevelKind = bkFlat
    BorderStyle = bsNone
    TabOrder = 2
  end
  object Panel1: TPanel
    Left = 0
    Top = 78
    Width = 464
    Height = 41
    Align = alBottom
    TabOrder = 1
    object CloseButton: TButton
      Left = 376
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Stop'
      ModalResult = 1
      TabOrder = 0
    end
    object PauseCheckBox: TCheckBox
      Left = 8
      Top = 8
      Width = 177
      Height = 17
      Caption = 'Pause on simulation completion.'
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = PauseCheckBoxClick
    end
  end
end
