object ScreenForm: TScreenForm
  Left = 299
  Top = 121
  BorderStyle = bsSingle
  Caption = 'APSIM'
  ClientHeight = 121
  ClientWidth = 464
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefault
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object StartDateLabel: TLabel
    Left = 16
    Top = 64
    Width = 3
    Height = 13
  end
  object EndDateLabel: TLabel
    Left = 445
    Top = 64
    Width = 3
    Height = 13
    Alignment = taRightJustify
  end
  object Label1: TLabel
    Left = 16
    Top = 8
    Width = 94
    Height = 13
    Caption = 'Simulation progress.'
  end
  object ErrorLabel: TLabel
    Left = 160
    Top = 64
    Width = 140
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = 'Errors were encountered'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    Visible = False
  end
  object FinishedLabel: TLabel
    Left = 160
    Top = 48
    Width = 140
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = 'APSIM has finished'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    Visible = False
  end
  object ProgressBar: TProgressBar
    Left = 16
    Top = 24
    Width = 433
    Height = 17
    Min = 0
    Max = 100
    Smooth = True
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
    Top = 80
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
      Default = True
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
