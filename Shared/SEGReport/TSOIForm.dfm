inherited SOIForm: TSOIForm
  Caption = 'SOIForm'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited PageControl1: TPageControl
    inherited PropertiesSheet: TTabSheet
      object Label3: TLabel
        Left = 48
        Top = 16
        Width = 86
        Height = 13
        Caption = 'SOI Month to use:'
      end
      object SOIList: TListBox
        Left = 48
        Top = 32
        Width = 89
        Height = 169
        ItemHeight = 13
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
        OnClick = SOIListClick
      end
      object NegativeCheckBox: TCheckBox
        Left = 48
        Top = 224
        Width = 97
        Height = 17
        Caption = 'Negative'
        TabOrder = 1
        OnClick = NegativeCheckBoxClick
      end
      object PositiveCheckBox: TCheckBox
        Left = 48
        Top = 248
        Width = 97
        Height = 17
        Caption = 'Positive'
        TabOrder = 2
        OnClick = PositiveCheckBoxClick
      end
      object FallingCheckBox: TCheckBox
        Left = 48
        Top = 272
        Width = 97
        Height = 17
        Caption = 'Falling'
        TabOrder = 3
        OnClick = FallingCheckBoxClick
      end
      object RisingCheckBox: TCheckBox
        Left = 48
        Top = 296
        Width = 97
        Height = 17
        Caption = 'Rising'
        TabOrder = 4
        OnClick = RisingCheckBoxClick
      end
      object ZeroCheckBox: TCheckBox
        Left = 48
        Top = 320
        Width = 97
        Height = 17
        Caption = 'Zero'
        TabOrder = 5
        OnClick = ZeroCheckBoxClick
      end
    end
  end
end
