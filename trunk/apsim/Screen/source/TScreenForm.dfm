object ScreenForm: TScreenForm
  Left = 301
  Top = 123
  BorderStyle = bsSingle
  Caption = 'APSIM'
  ClientHeight = 137
  ClientWidth = 467
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Icon.Data = {
    0000010001002020100000000000E80200001600000028000000200000004000
    0000010004000000000080020000000000000000000000000000000000000000
    000000008000008000000080800080000000800080008080000080808000C0C0
    C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    00000000000000000000000004CE00000000000000000000000000004CEE0000
    000000000000000000000004CEEC000000000000000000000000004CEECC0000
    0000000000000000000004CEECC00000000000000000000000004CEECC030000
    00000000000000000007CEECC030000000000000077700007878FECC03000000
    7700000000066666078F87C03000000AA7770000768E8E8E60087703A000000A
    BA77770768E8E8E8E660703AA00000AAAAAAAA778EFE8E8E8E6603B8AA0000AA
    BBBAAA78EFE8E8E8E8E607BBAA7700AAAAAAAA7EFEFE8E8E8E8603BABAA70ABA
    BBBBBA78EFF8E8E8E8E607BBBBA70AAAAAAAAA7EFEFF8E8E8E8603AAAAA70ABA
    BBABAA78EFFFE8E8E8E607BBBAB70AAAAAAAAA07FEFFFE8E8E8038BAAAA70AAA
    AAAAAAA08FEFEFE8E8603BABBBB70AAAAAAAAAAA78FEFEFE860ABABAB77000AA
    AAAAAAAAA778E8E700AAABBA7700000AAAAAAAAAAAA77770AAAABABA70000000
    AAAAAAAAAAAAAAAAAAAAAAA7700000000AAAAAAAAAAAAAAAAAAAAA7700000000
    00AAAAAAAAAAAAAAAAAAAA7700000000000AAAAAAAAAAAAAAAAAAA7000000000
    000AAAAAAAAAAA770AAAAA70000000000000000AAAAAA7700AAA777000000000
    00000000AAAAA0000AAA700000000000000000000AAAA00000AA700000000000
    000000000000000000AA7000000000000000000000000000000A00000000FFFF
    FFF8FFFFFFF0FFFFFFE0FFFFFFC0FFFFFF80FFFFFF00FFFFFE01FFF80003F3F8
    0007E0F00007E0200007C0000003C0000000C000000080000000800000008000
    0000802000008010000080000001C0000003E0000007F0000007F800000FFC00
    000FFE00001FFE00081FFFE0181FFFF0787FFFF87C7FFFFFFC7FFFFFFEFF}
  OldCreateOrder = False
  Position = poDefault
  Scaled = False
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object CurrentDateLabel: TLabel
    Left = 16
    Top = 52
    Width = 433
    Height = 13
    Alignment = taCenter
    AutoSize = False
  end
  object StartDateLabel: TLabel
    Left = 16
    Top = 51
    Width = 3
    Height = 13
  end
  object EndDateLabel: TLabel
    Left = 445
    Top = 50
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
    Left = 374
    Top = 80
    Width = 77
    Height = 13
    Alignment = taRightJustify
    Caption = '(errors found)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    Visible = False
  end
  object FinishedLabel: TLabel
    Left = 184
    Top = 80
    Width = 111
    Height = 13
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
    Height = 25
    Min = 0
    Max = 30
    Smooth = True
    Step = 1
    TabOrder = 0
  end
  object Memo: TMemo
    Left = 16
    Top = 96
    Width = 436
    Height = 289
    BevelInner = bvNone
    BevelKind = bkFlat
    BorderStyle = bsNone
    TabOrder = 2
  end
  object Panel1: TPanel
    Left = 0
    Top = 96
    Width = 467
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
      OnClick = CloseButtonClick
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
