inherited ShapeForm: TShapeForm
  Caption = 'ShapeForm'
  OldCreateOrder = True
  PixelsPerInch = 120
  TextHeight = 16
  object Label3: TLabel [3]
    Left = 11
    Top = 160
    Width = 67
    Height = 16
    Caption = 'Pen colour:'
  end
  object Label4: TLabel [4]
    Left = 1
    Top = 190
    Width = 77
    Height = 16
    Caption = 'Brush colour:'
  end
  object Label5: TLabel [5]
    Left = 36
    Top = 130
    Width = 43
    Height = 16
    Caption = 'Shape:'
  end
  object BrushColourCombo: TColorBox
    Left = 89
    Top = 190
    Width = 178
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    BevelKind = bkSoft
    ItemHeight = 16
    TabOrder = 6
    OnChange = BrushColourComboChange
  end
  object PenColourCombo: TColorBox
    Left = 89
    Top = 160
    Width = 178
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    BevelKind = bkSoft
    ItemHeight = 16
    TabOrder = 5
    OnChange = PenColourComboChange
  end
  object ShapeCombo: TComboBox
    Left = 89
    Top = 130
    Width = 178
    Height = 24
    BevelKind = bkSoft
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 16
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
