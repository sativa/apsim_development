inherited ExcelForm: TExcelForm
  Caption = 'ExcelForm'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited PageControl1: TPageControl
    inherited PropertiesSheet: TTabSheet
      object Label3: TLabel
        Left = 0
        Top = 8
        Width = 45
        Height = 13
        Caption = 'Filename:'
      end
      object Label4: TLabel
        Left = 0
        Top = 48
        Width = 55
        Height = 13
        Caption = 'Experiment:'
      end
      object FilenameEdit: TAdvFileNameEdit
        Left = 0
        Top = 24
        Width = 193
        Height = 21
        AutoFocus = False
        ErrorMarkerPos = 0
        ErrorMarkerLen = 0
        ErrorColor = clRed
        ErrorFontColor = clWhite
        Flat = False
        FlatLineColor = clBlack
        FlatParentColor = True
        FocusAlign = eaDefault
        FocusBorder = False
        FocusColor = clWindow
        FocusFontColor = clWindowText
        FocusLabel = False
        FocusWidthInc = 0
        ModifiedColor = clHighlight
        DisabledColor = clSilver
        ReturnIsTab = False
        LengthLimit = 0
        TabOnFullLength = False
        LabelPosition = lpLeftTop
        LabelMargin = 4
        LabelTransparent = False
        LabelAlwaysEnabled = False
        LabelFont.Charset = DEFAULT_CHARSET
        LabelFont.Color = clWindowText
        LabelFont.Height = -11
        LabelFont.Name = 'MS Sans Serif'
        LabelFont.Style = []
        Persistence.Enable = False
        Persistence.Location = plInifile
        Anchors = [akLeft, akTop, akRight]
        Color = clWindow
        Enabled = True
        HintShowLargeText = False
        OleDropTarget = False
        OleDropSource = False
        TabOrder = 0
        Transparent = False
        Visible = True
        OnChange = FilenameEditChange
        ButtonWidth = 18
        Etched = False
        Glyph.Data = {
          CE000000424DCE0000000000000076000000280000000C0000000B0000000100
          0400000000005800000000000000000000001000000000000000000000000000
          8000008000000080800080000000800080008080000080808000C0C0C0000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00D00000000DDD
          00000077777770DD00000F077777770D00000FF07777777000000FFF00000000
          00000FFFFFFF0DDD00000FFF00000DDD0000D000DDDDD0000000DDDDDDDDDD00
          0000DDDDD0DDD0D00000DDDDDD000DDD0000}
        DefaultExt = 'xls'
        Filter = 'Excel files(*.xls)|*.xls'
        FilterIndex = 0
        DialogOptions = []
        DialogKind = fdOpen
      end
      object PageCombo: TComboBox
        Left = 0
        Top = 64
        Width = 193
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        TabOrder = 1
        OnChange = PageComboChange
      end
    end
  end
end
