inherited StatsForm: TStatsForm
  Caption = 'StatsForm'
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
      object FieldNameCombo: TComboBox
        Left = 8
        Top = 32
        Width = 209
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        TabOrder = 0
        OnChange = FieldNameComboChange
      end
      object MeanCheckBox: TCheckBox
        Left = 8
        Top = 64
        Width = 97
        Height = 17
        Caption = 'Mean'
        TabOrder = 1
        OnClick = CheckBoxClick
      end
      object MinCheckBox: TCheckBox
        Left = 8
        Top = 88
        Width = 97
        Height = 17
        Caption = 'Minimum'
        TabOrder = 2
        OnClick = CheckBoxClick
      end
      object MaxCheckBox: TCheckBox
        Left = 8
        Top = 112
        Width = 97
        Height = 17
        Caption = 'Maximum'
        TabOrder = 3
        OnClick = CheckBoxClick
      end
      object CountCheckBox: TCheckBox
        Left = 8
        Top = 136
        Width = 97
        Height = 17
        Caption = 'Count'
        TabOrder = 4
        OnClick = CheckBoxClick
      end
      object GroupBox1: TGroupBox
        Left = 8
        Top = 168
        Width = 185
        Height = 129
        Caption = 'Deciles'
        TabOrder = 5
        object Decile60CheckBox: TCheckBox
          Left = 112
          Top = 24
          Width = 57
          Height = 17
          Caption = '60%'
          TabOrder = 0
          OnClick = CheckBoxClick
        end
        object Decile70CheckBox: TCheckBox
          Left = 112
          Top = 48
          Width = 57
          Height = 17
          Caption = '70%'
          TabOrder = 1
          OnClick = CheckBoxClick
        end
        object Decile80CheckBox: TCheckBox
          Left = 112
          Top = 72
          Width = 57
          Height = 17
          Caption = '80%'
          TabOrder = 2
          OnClick = CheckBoxClick
        end
        object Decile90CheckBox: TCheckBox
          Left = 112
          Top = 96
          Width = 57
          Height = 17
          Caption = '90%'
          TabOrder = 3
          OnClick = CheckBoxClick
        end
        object Decile10CheckBox: TCheckBox
          Left = 8
          Top = 24
          Width = 57
          Height = 17
          Caption = '10%'
          TabOrder = 4
          OnClick = CheckBoxClick
        end
        object Decile20CheckBox: TCheckBox
          Left = 8
          Top = 48
          Width = 57
          Height = 17
          Caption = '20%'
          TabOrder = 5
          OnClick = CheckBoxClick
        end
        object Decile30CheckBox: TCheckBox
          Left = 8
          Top = 72
          Width = 57
          Height = 17
          Caption = '30%'
          TabOrder = 6
          OnClick = CheckBoxClick
        end
        object Decile40CheckBox: TCheckBox
          Left = 8
          Top = 96
          Width = 57
          Height = 17
          Caption = '40%'
          TabOrder = 7
          OnClick = CheckBoxClick
        end
        object Decile50CheckBox: TCheckBox
          Left = 64
          Top = 64
          Width = 49
          Height = 17
          Caption = '50%'
          TabOrder = 8
          OnClick = CheckBoxClick
        end
      end
    end
  end
  inherited GridDataSource: TDataSource
    Left = 182
    Top = 10
  end
end
