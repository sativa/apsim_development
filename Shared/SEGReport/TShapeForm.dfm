object ShapeForm: TShapeForm
  Left = 250
  Top = 114
  Width = 260
  Height = 361
  Caption = 'ShapeForm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 252
    Height = 327
    ActivePage = Properties
    Align = alClient
    TabIndex = 0
    TabOrder = 0
    object Properties: TTabSheet
      Caption = 'Properties'
      object Label1: TLabel
        Left = 28
        Top = 24
        Width = 34
        Height = 13
        Caption = 'Shape:'
      end
      object Label2: TLabel
        Left = 8
        Top = 48
        Width = 54
        Height = 13
        Caption = 'Pen colour:'
      end
      object Label4: TLabel
        Left = 0
        Top = 72
        Width = 62
        Height = 13
        Caption = 'Brush colour:'
      end
      object ShapeCombo: TComboBox
        Left = 72
        Top = 24
        Width = 129
        Height = 21
        ItemHeight = 13
        TabOrder = 0
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
      object PenColourCombo: TAdvColorComboBox
        Left = 72
        Top = 48
        Width = 129
        Height = 22
        AutoFocus = False
        ButtonWidth = 21
        Style = csOwnerDrawFixed
        Flat = False
        Etched = False
        FocusBorder = False
        DropWidth = 0
        ItemHeight = 16
        Items.Strings = (
          ''
          ''
          ''
          ''
          ''
          ''
          ''
          ''
          ''
          ''
          ''
          ''
          ''
          ''
          ''
          '')
        TabOrder = 1
        OnChange = PenColourComboChange
      end
      object BrushColourCombo: TAdvColorComboBox
        Left = 72
        Top = 72
        Width = 129
        Height = 22
        AutoFocus = False
        ButtonWidth = 21
        Style = csOwnerDrawFixed
        Flat = False
        Etched = False
        FocusBorder = False
        DropWidth = 0
        ItemHeight = 16
        Items.Strings = (
          ''
          ''
          ''
          ''
          ''
          ''
          ''
          ''
          ''
          ''
          ''
          ''
          ''
          ''
          ''
          '')
        TabOrder = 2
        OnChange = BrushColourComboChange
      end
    end
    object TabSheet1: TTabSheet
      Caption = 'Advanced'
      ImageIndex = 1
      object Label3: TLabel
        Left = 8
        Top = 8
        Width = 31
        Height = 13
        Caption = 'Name:'
      end
      object NameEdit: TEdit
        Left = 40
        Top = 8
        Width = 121
        Height = 21
        TabOrder = 0
      end
    end
  end
end
