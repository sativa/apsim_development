object AboutForm: TAboutForm
  Left = 333
  Top = 152
  AutoScroll = False
  Caption = 'About APSIM'
  ClientHeight = 346
  ClientWidth = 355
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label3: TLabel
    Left = 0
    Top = 0
    Width = 355
    Height = 52
    Align = alTop
    Alignment = taCenter
    Caption = 'APSIM'
    Color = clBtnFace
    Font.Charset = ANSI_CHARSET
    Font.Color = clGreen
    Font.Height = -37
    Font.Name = 'Comic Sans MS'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object Label5: TLabel
    Left = 0
    Top = 52
    Width = 355
    Height = 37
    Align = alTop
    Alignment = taCenter
    AutoSize = False
    Caption = 'Copyright '#169' 1991-2002 APSRU'
    Color = clBtnFace
    Font.Charset = ANSI_CHARSET
    Font.Color = clGreen
    Font.Height = -19
    Font.Name = 'Comic Sans MS'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object Panel1: TPanel
    Left = 0
    Top = 294
    Width = 355
    Height = 52
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      355
      52)
    object Label4: TLabel
      Left = 14
      Top = 10
      Width = 89
      Height = 13
      Alignment = taRightJustify
      Caption = 'APSIM Help WEB:'
    end
    object WEBLabel: TLabel
      Left = 106
      Top = 10
      Width = 144
      Height = 13
      Cursor = crHandPoint
      Caption = 'http://apsim-help.tag.csiro.au/'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsUnderline]
      ParentFont = False
      OnClick = WEBLabelClick
    end
    object Label6: TLabel
      Left = 15
      Top = 26
      Width = 88
      Height = 13
      Alignment = taRightJustify
      Caption = 'APSIM Help email:'
    end
    object EmailLabel: TLabel
      Left = 106
      Top = 26
      Width = 124
      Height = 13
      Cursor = crHandPoint
      Caption = 'APSIM-Help@tag.csiro.au'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsUnderline]
      ParentFont = False
      OnClick = EmailLabelClick
    end
    object BitBtn1: TBitBtn
      Left = 266
      Top = 15
      Width = 73
      Height = 24
      Anchors = [akRight, akBottom]
      TabOrder = 0
      Kind = bkOK
    end
  end
  object ScrollBox1: TScrollBox
    Left = 0
    Top = 89
    Width = 355
    Height = 205
    Align = alClient
    TabOrder = 1
    object DetailsLabel: TParamLabel
      Left = 1
      Top = 1
      Width = 6
      Height = 19
      AutoSizing = True
      Hover = False
      HoverColor = clNone
      HoverFontColor = clNone
      ParamColor = clGreen
      ParamHint = False
      ShadowColor = clGray
      ShadowOffset = 2
    end
  end
end
