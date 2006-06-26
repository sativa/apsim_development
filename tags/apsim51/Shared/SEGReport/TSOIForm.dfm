inherited SOIForm: TSOIForm
  Left = 1632
  Top = 232
  Caption = 'SOIForm'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited AdvPanelGroup1: TAdvPanelGroup
    FullHeight = 18
    inherited AdvancedPanel: TAdvPanel
      FullHeight = 293
    end
    inherited PropertyPanel: TAdvPanel
      FullHeight = 297
      object Label8: TLabel
        Left = 13
        Top = 26
        Width = 39
        Height = 13
        Caption = 'Month:'
      end
      object Label1: TLabel
        Left = 34
        Top = 287
        Width = 98
        Height = 13
        Caption = 'from source data'
      end
      object ZeroCheckBox: TCheckBox
        Left = 13
        Top = 176
        Width = 163
        Height = 17
        Caption = 'Zero SOI Phase'
        TabOrder = 0
        OnClick = ZeroCheckBoxClick
      end
      object RisingCheckBox: TCheckBox
        Left = 13
        Top = 150
        Width = 163
        Height = 17
        Caption = 'Rising SOI Phase'
        TabOrder = 1
        OnClick = RisingCheckBoxClick
      end
      object FallingCheckBox: TCheckBox
        Left = 13
        Top = 124
        Width = 163
        Height = 17
        Caption = 'Falling SOI Phase'
        TabOrder = 2
        OnClick = FallingCheckBoxClick
      end
      object PositiveCheckBox: TCheckBox
        Left = 13
        Top = 98
        Width = 163
        Height = 16
        Caption = 'Positive SOI Phase'
        TabOrder = 3
        OnClick = PositiveCheckBoxClick
      end
      object NegativeCheckBox: TCheckBox
        Left = 13
        Top = 72
        Width = 163
        Height = 17
        Caption = 'Negative SOI Phase'
        TabOrder = 4
        OnClick = NegativeCheckBoxClick
      end
      object MonthCombo: TComboBox
        Left = 13
        Top = 39
        Width = 145
        Height = 21
        BevelKind = bkSoft
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        TabOrder = 5
        OnClick = MonthComboClick
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
      end
      object GetFromSourceCheckBox: TCheckBox
        Left = 16
        Top = 264
        Width = 257
        Height = 25
        Caption = 'Get SOI phase and month'
        TabOrder = 6
        OnClick = GetFromSourceCheckBoxClick
      end
    end
  end
end
