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
    end
  end
end
