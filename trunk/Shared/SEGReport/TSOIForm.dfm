inherited SOIForm: TSOIForm
  Caption = 'SOIForm'
  OldCreateOrder = True
  PixelsPerInch = 120
  TextHeight = 16
  inherited AdvPanelGroup1: TAdvPanelGroup
    FullHeight = 18
    inherited AdvancedPanel: TAdvPanel
      FullHeight = 293
    end
    inherited PropertyPanel: TAdvPanel
      FullHeight = 297
      object Label8: TLabel
        Left = 16
        Top = 32
        Width = 47
        Height = 16
        Caption = 'Month:'
      end
      object ZeroCheckBox: TCheckBox
        Left = 16
        Top = 216
        Width = 201
        Height = 21
        Caption = 'Zero SOI Phase'
        TabOrder = 0
        OnClick = ZeroCheckBoxClick
      end
      object RisingCheckBox: TCheckBox
        Left = 16
        Top = 184
        Width = 201
        Height = 21
        Caption = 'Rising SOI Phase'
        TabOrder = 1
        OnClick = RisingCheckBoxClick
      end
      object FallingCheckBox: TCheckBox
        Left = 16
        Top = 152
        Width = 201
        Height = 21
        Caption = 'Falling SOI Phase'
        TabOrder = 2
        OnClick = FallingCheckBoxClick
      end
      object PositiveCheckBox: TCheckBox
        Left = 16
        Top = 120
        Width = 201
        Height = 20
        Caption = 'Positive SOI Phase'
        TabOrder = 3
        OnClick = PositiveCheckBoxClick
      end
      object NegativeCheckBox: TCheckBox
        Left = 16
        Top = 88
        Width = 201
        Height = 21
        Caption = 'Negative SOI Phase'
        TabOrder = 4
        OnClick = NegativeCheckBoxClick
      end
      object MonthCombo: TComboBox
        Left = 16
        Top = 48
        Width = 178
        Height = 24
        BevelKind = bkSoft
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 16
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
    end
  end
end
