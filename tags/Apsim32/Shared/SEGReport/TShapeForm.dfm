inherited ShapeForm: TShapeForm
  Caption = 'ShapeForm'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object Label3: TLabel [3]
    Left = 9
    Top = 104
    Width = 54
    Height = 13
    Caption = 'Pen colour:'
  end
  object Label4: TLabel [4]
    Left = 1
    Top = 128
    Width = 62
    Height = 13
    Caption = 'Brush colour:'
  end
  object Label5: TLabel [5]
    Left = 29
    Top = 80
    Width = 34
    Height = 13
    Caption = 'Shape:'
  end
  inherited ToolbarCheckBox: TCheckBox
    TabOrder = 5
  end
  object BrushColourCombo: TColorBox
    Left = 72
    Top = 128
    Width = 145
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    BevelKind = bkSoft
    ItemHeight = 16
    TabOrder = 2
    OnChange = BrushColourComboChange
  end
  object PenColourCombo: TColorBox
    Left = 72
    Top = 104
    Width = 145
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    BevelKind = bkSoft
    ItemHeight = 16
    TabOrder = 3
    OnChange = PenColourComboChange
  end
  object ShapeCombo: TComboBox
    Left = 72
    Top = 80
    Width = 145
    Height = 21
    BevelKind = bkSoft
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 4
    Text = 'Rectangle'
    OnChange = ShapeComboChange
    Items.Strings = (
      'Rectangle'
      'Circle'
      'Vertical line'
      'Horizontal line'
      'Top and Bottom'
      'Right and Left')
  end
end
