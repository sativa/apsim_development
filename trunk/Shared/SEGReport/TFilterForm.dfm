inherited FilterForm: TFilterForm
  Caption = 'Filter form'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited PageControl1: TPageControl
    inherited PropertiesSheet: TTabSheet
      OnShow = PropertiesSheetShow
      object Label3: TLabel
        Left = 0
        Top = 32
        Width = 67
        Height = 13
        Caption = 'Column name:'
      end
      object Label4: TLabel
        Left = 0
        Top = 72
        Width = 67
        Height = 13
        Caption = 'Column value:'
      end
      object FieldNameCombo: TComboBox
        Left = 72
        Top = 32
        Width = 145
        Height = 21
        ItemHeight = 13
        TabOrder = 0
        OnChange = FieldNameComboChange
      end
      object FieldValueCombo: TComboBox
        Left = 72
        Top = 72
        Width = 145
        Height = 21
        ItemHeight = 13
        TabOrder = 1
        OnChange = FieldValueComboChange
      end
    end
  end
end
