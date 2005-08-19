inherited REMSForm: TREMSForm
  Caption = 'REMSForm'
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
        Top = 32
        Width = 63
        Height = 16
        Caption = 'Filename:'
      end
      object Label4: TLabel
        Left = 16
        Top = 80
        Width = 78
        Height = 16
        Caption = 'Experiment:'
      end
      object Label5: TLabel
        Left = 16
        Top = 136
        Width = 75
        Height = 16
        Caption = 'Treatment:'
      end
      object Label6: TLabel
        Left = 16
        Top = 192
        Width = 88
        Height = 16
        Caption = 'Data Source:'
      end
      object DataSourceCombo: TComboBox
        Left = 16
        Top = 208
        Width = 233
        Height = 24
        BevelKind = bkSoft
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 16
        TabOrder = 0
        Text = 'Crop'
        OnChange = DataSourceComboChange
        Items.Strings = (
          'Crop'
          'Soil Layered'
          'Statistics')
      end
      object TreatmentCombo: TComboBox
        Left = 16
        Top = 152
        Width = 233
        Height = 24
        BevelKind = bkSoft
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 16
        TabOrder = 1
        OnChange = TreatmentComboChange
      end
      object ExperimentCombo: TComboBox
        Left = 16
        Top = 96
        Width = 233
        Height = 24
        BevelKind = bkSoft
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 16
        TabOrder = 2
        OnChange = ExperimentComboChange
      end
      object FilenameEdit: TAdvFileNameEdit
        Left = 16
        Top = 48
        Width = 233
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
        Lookup.CaseSensitive = False
        Lookup.Color = clWindow
        Lookup.DisplayCount = 4
        Lookup.Enabled = False
        Lookup.NumChars = 2
        Persistence.Enable = False
        Persistence.Location = plInifile
        BevelKind = bkSoft
        Anchors = [akLeft, akTop, akRight]
        BorderStyle = bsNone
        Color = clWindow
        Enabled = True
        HintShowLargeText = False
        OleDropTarget = False
        OleDropSource = False
        TabOrder = 3
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
        DefaultExt = 'mdb'
        Filter = 'MDB files(*.mdb)|*.mdb'
        FilterIndex = 0
        DialogOptions = []
        DialogKind = fdOpen
      end
    end
  end
end
