object OutlookSplashForm: TOutlookSplashForm
  Left = 1086
  Top = 85
  BorderIcons = []
  BorderStyle = bsNone
  Caption = ' '
  ClientHeight = 184
  ClientWidth = 288
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  OnShow = FormShow
  DesignSize = (
    288
    184)
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 0
    Top = 0
    Width = 288
    Height = 144
    Anchors = [akLeft, akTop, akRight, akBottom]
    AutoSize = True
  end
  object Panel1: TPanel
    Left = 0
    Top = 144
    Width = 288
    Height = 40
    Anchors = [akLeft, akRight, akBottom]
    BevelOuter = bvNone
    Ctl3D = True
    ParentCtl3D = False
    TabOrder = 0
    DesignSize = (
      288
      40)
    object OkButton: TButton
      Left = 201
      Top = 7
      Width = 73
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'Ok'
      Default = True
      ModalResult = 1
      TabOrder = 0
      OnClick = OkButtonClick
    end
  end
  object Timer1: TTimer
    Interval = 4000
    OnTimer = Timer1Timer
    Left = 40
    Top = 16
  end
end
