inherited StatsForm: TStatsForm
  Height = 557
  Caption = 'StatsForm'
  OldCreateOrder = True
  PixelsPerInch = 120
  TextHeight = 16
  inherited AdvPanelGroup1: TAdvPanelGroup
    Height = 523
    FullHeight = 18
    inherited AdvancedPanel: TAdvPanel
      FullHeight = 293
    end
    inherited PropertyPanel: TAdvPanel
      FullHeight = 297
      object Label3: TLabel
        Left = 16
        Top = 32
        Width = 76
        Height = 16
        Caption = 'Field name:'
      end
      object Decile90CheckBox: TCheckBox
        Left = 168
        Top = 280
        Width = 70
        Height = 21
        Caption = '90%'
        TabOrder = 0
        OnClick = CheckBoxClick
      end
      object Decile80CheckBox: TCheckBox
        Left = 168
        Top = 256
        Width = 70
        Height = 21
        Caption = '80%'
        TabOrder = 1
        OnClick = CheckBoxClick
      end
      object Decile70CheckBox: TCheckBox
        Left = 168
        Top = 232
        Width = 70
        Height = 21
        Caption = '70%'
        TabOrder = 2
        OnClick = CheckBoxClick
      end
      object Decile60CheckBox: TCheckBox
        Left = 168
        Top = 208
        Width = 70
        Height = 21
        Caption = '60%'
        TabOrder = 3
        OnClick = CheckBoxClick
      end
      object Decile50CheckBox: TCheckBox
        Left = 168
        Top = 184
        Width = 60
        Height = 21
        Caption = '50%'
        TabOrder = 4
        OnClick = CheckBoxClick
      end
      object Decile40CheckBox: TCheckBox
        Left = 168
        Top = 160
        Width = 70
        Height = 20
        Caption = '40%'
        TabOrder = 5
        OnClick = CheckBoxClick
      end
      object Decile30CheckBox: TCheckBox
        Left = 168
        Top = 136
        Width = 70
        Height = 21
        Caption = '30%'
        TabOrder = 6
        OnClick = CheckBoxClick
      end
      object Decile20CheckBox: TCheckBox
        Left = 168
        Top = 112
        Width = 70
        Height = 21
        Caption = '20%'
        TabOrder = 7
        OnClick = CheckBoxClick
      end
      object Decile10CheckBox: TCheckBox
        Left = 168
        Top = 88
        Width = 70
        Height = 21
        Caption = '10%'
        TabOrder = 8
        OnClick = CheckBoxClick
      end
      object SumCheckBox: TCheckBox
        Left = 16
        Top = 184
        Width = 119
        Height = 21
        Caption = 'Sum'
        TabOrder = 9
        OnClick = CheckBoxClick
      end
      object CountCheckBox: TCheckBox
        Left = 16
        Top = 160
        Width = 119
        Height = 21
        Caption = 'Count'
        TabOrder = 10
        OnClick = CheckBoxClick
      end
      object MaxCheckBox: TCheckBox
        Left = 16
        Top = 136
        Width = 119
        Height = 21
        Caption = 'Maximum'
        TabOrder = 11
        OnClick = CheckBoxClick
      end
      object MinCheckBox: TCheckBox
        Left = 16
        Top = 112
        Width = 119
        Height = 21
        Caption = 'Minimum'
        TabOrder = 12
        OnClick = CheckBoxClick
      end
      object MeanCheckBox: TCheckBox
        Left = 16
        Top = 88
        Width = 119
        Height = 21
        Caption = 'Mean'
        TabOrder = 13
        OnClick = CheckBoxClick
      end
      object FieldNameCombo: TComboBox
        Left = 16
        Top = 48
        Width = 233
        Height = 24
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 16
        TabOrder = 14
        OnChange = FieldNameComboChange
      end
    end
  end
end
