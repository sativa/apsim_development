inherited Difference_form: TDifference_form
  Caption = 'Difference'
  PixelsPerInch = 96
  TextHeight = 16
  object Label3: TLabel [1]
    Left = 188
    Top = 34
    Width = 261
    Height = 16
    Caption = 'Perform a difference on which 2 simulations?'
  end
  inherited BitBtn2: TBitBtn
    TabOrder = 4
  end
  inherited VariableList: TListView
    Width = 281
    TabOrder = 5
  end
  object GroupBox1: TGroupBox [9]
    Left = 296
    Top = 216
    Width = 321
    Height = 73
    Caption = 'Pair1'
    TabOrder = 1
    object LHS1: TComboBox
      Left = 8
      Top = 18
      Width = 305
      Height = 24
      ItemHeight = 16
      TabOrder = 0
      OnChange = Combo_change
    end
    object RHS1: TComboBox
      Left = 8
      Top = 42
      Width = 305
      Height = 24
      ItemHeight = 16
      TabOrder = 1
      OnChange = Combo_change
    end
  end
  object GroupBox2: TGroupBox [10]
    Left = 296
    Top = 296
    Width = 321
    Height = 78
    Caption = 'Pair2'
    TabOrder = 2
    Visible = False
    object LHS2: TComboBox
      Left = 8
      Top = 20
      Width = 305
      Height = 24
      ItemHeight = 16
      TabOrder = 0
      OnChange = Combo_change
    end
    object RHS2: TComboBox
      Left = 8
      Top = 44
      Width = 305
      Height = 24
      ItemHeight = 16
      TabOrder = 1
      OnChange = Combo_change
    end
  end
end
