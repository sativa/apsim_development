inherited ShapeForm: TShapeForm
  Caption = 'ShapeForm'
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
      object Label3: TLabel
        Left = 16
        Top = 88
        Width = 75
        Height = 16
        Caption = 'Pen colour:'
      end
      object Label4: TLabel
        Left = 16
        Top = 144
        Width = 87
        Height = 16
        Caption = 'Brush colour:'
      end
      object Label5: TLabel
        Left = 16
        Top = 32
        Width = 47
        Height = 16
        Caption = 'Shape:'
      end
      object BrushColourCombo: TColorBox
        Left = 16
        Top = 160
        Width = 233
        Height = 22
        Anchors = [akLeft, akTop, akRight]
        BevelKind = bkSoft
        ItemHeight = 16
        TabOrder = 0
        OnChange = BrushColourComboChange
      end
      object PenColourCombo: TColorBox
        Left = 16
        Top = 104
        Width = 233
        Height = 22
        Anchors = [akLeft, akTop, akRight]
        BevelKind = bkSoft
        ItemHeight = 16
        TabOrder = 1
        OnChange = PenColourComboChange
      end
      object ShapeCombo: TComboBox
        Left = 16
        Top = 48
        Width = 233
        Height = 24
        BevelKind = bkSoft
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 16
        TabOrder = 2
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
  end
end
