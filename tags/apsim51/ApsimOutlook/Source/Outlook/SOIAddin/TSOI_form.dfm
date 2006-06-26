object SOI_form: TSOI_form
  Left = 387
  Top = 220
  AutoScroll = False
  Caption = 'SOI phase month selection'
  ClientHeight = 609
  ClientWidth = 313
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 16
  object SOI_listbox: TListBox
    Left = 96
    Top = 117
    Width = 105
    Height = 204
    ItemHeight = 16
    Items.Strings = (
      'January'
      'February'
      'March'
      'April'
      'May'
      'June'
      'July'
      'August'
      'September'
      'October'
      'November'
      'December')
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 568
    Width = 313
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object BitBtn1: TBitBtn
      Left = 58
      Top = 3
      Width = 92
      Height = 30
      TabOrder = 0
      Kind = bkOK
    end
    object BitBtn2: TBitBtn
      Left = 165
      Top = 3
      Width = 92
      Height = 30
      TabOrder = 1
      Kind = bkCancel
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 33
    Width = 313
    Height = 84
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object Label1: TLabel
      Left = 11
      Top = 9
      Width = 275
      Height = 16
      Caption = 'Select the SOI phase month from the list below:'
    end
    object Label2: TLabel
      Left = 41
      Top = 39
      Width = 219
      Height = 16
      Caption = 'Eg: Selecting September will use the '
    end
    object Label3: TLabel
      Left = 58
      Top = 59
      Width = 184
      Height = 16
      Caption = 'August - September SOI phase'
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 0
    Width = 313
    Height = 33
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 3
    object SOI_toggle: TCheckBox
      Left = 11
      Top = 8
      Width = 129
      Height = 17
      Caption = 'Turn SOI on'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = SOI_toggleClick
    end
  end
  object AllYears: TCheckBox
    Left = 64
    Top = 520
    Width = 169
    Height = 17
    Caption = 'All years'
    Checked = True
    State = cbChecked
    TabOrder = 4
  end
  object PhaseCheckBox: TCheckListBox
    Left = 64
    Top = 336
    Width = 169
    Height = 177
    ItemHeight = 16
    TabOrder = 5
  end
end
