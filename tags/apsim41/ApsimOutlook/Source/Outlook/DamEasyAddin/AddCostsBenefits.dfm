object AddCostsBenefitsForm: TAddCostsBenefitsForm
  Left = 278
  Top = 355
  BorderStyle = bsDialog
  Caption = 'Additional Costs and Benefits Analysis'
  ClientHeight = 270
  ClientWidth = 413
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  DesignSize = (
    413
    270)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 114
    Width = 100
    Height = 13
    Caption = 'Benchmark scenario:'
  end
  object Label2: TLabel
    Left = 16
    Top = 169
    Width = 88
    Height = 13
    Caption = 'Investment Period:'
  end
  object Label3: TLabel
    Left = 17
    Top = 202
    Width = 68
    Height = 13
    Caption = 'Salvage Rate:'
  end
  object Label4: TLabel
    Left = 204
    Top = 202
    Width = 8
    Height = 13
    Caption = '%'
  end
  object Label5: TLabel
    Left = 204
    Top = 169
    Width = 25
    Height = 13
    Caption = 'years'
  end
  object AnalysisTypeRadioGroup: TRadioGroup
    Left = 16
    Top = 8
    Width = 265
    Height = 97
    Caption = 'Type of Analysis Required:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Items.Strings = (
      'Cash Flow Analysis'
      'Additional Costs and Benefits'
      'Net Present Value and Internal Rate of Return')
    ParentFont = False
    TabOrder = 0
    OnClick = AnalysisTypeRadioGroupClick
  end
  object BaseCaseComboBox: TComboBox
    Left = 16
    Top = 132
    Width = 382
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 1
  end
  object InvestmentPeriodBox: TEdit
    Left = 112
    Top = 165
    Width = 89
    Height = 21
    TabOrder = 2
    Text = 'InvestmentPeriodBox'
    OnExit = InvestmentPeriodBoxExit
  end
  object OKButton: TBitBtn
    Left = 227
    Top = 238
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    TabOrder = 4
    Kind = bkOK
  end
  object CancelButton: TBitBtn
    Left = 318
    Top = 238
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    TabOrder = 5
    Kind = bkCancel
  end
  object SalvageRateBox: TEdit
    Left = 112
    Top = 198
    Width = 89
    Height = 21
    TabOrder = 3
    Text = 'SalvageRateBox'
  end
end
