inherited ProbabilityForm: TProbabilityForm
  Caption = 'ProbabilityForm'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited PageControl1: TPageControl
    inherited PropertiesSheet: TTabSheet
      OnShow = PropertiesSheetShow
      object Label3: TLabel
        Left = 8
        Top = 16
        Width = 54
        Height = 13
        Caption = 'Field name:'
      end
      object GroupBox1: TGroupBox
        Left = 32
        Top = 72
        Width = 129
        Height = 81
        Caption = 'Probability type:'
        TabOrder = 0
        object ExceedenceRadio: TRadioButton
          Left = 8
          Top = 24
          Width = 113
          Height = 17
          Caption = 'Exceedence'
          Checked = True
          TabOrder = 0
          TabStop = True
          OnClick = ExceedenceRadioClick
        end
        object CumulativeRadio: TRadioButton
          Left = 8
          Top = 48
          Width = 113
          Height = 17
          Caption = 'Cumulative'
          TabOrder = 1
          OnClick = CumulativeRadioClick
        end
      end
      object FieldNameCombo: TComboBox
        Left = 8
        Top = 32
        Width = 209
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        TabOrder = 1
        OnChange = FieldNameComboChange
      end
    end
  end
end
