object GMForm: TGMForm
  Left = 214
  Top = 131
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Economic Configuration'
  ClientHeight = 573
  ClientWidth = 691
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label26: TLabel
    Left = 8
    Top = 40
    Width = 50
    Height = 13
    Caption = 'Scenarios:'
  end
  object GMInfo: TPageControl
    Left = 184
    Top = 56
    Width = 497
    Height = 481
    ActivePage = CropCostPage
    TabIndex = 0
    TabOrder = 0
    OnChanging = GMInfoChanging
    object ScenarioPage: TTabSheet
      Caption = 'ScenarioPage'
      TabVisible = False
      object Label12: TLabel
        Left = 8
        Top = 64
        Width = 36
        Height = 16
        Caption = 'Notes'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clRed
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object ScenarioLabel: TLabel
        Left = 8
        Top = 40
        Width = 102
        Height = 16
        Caption = 'ScenarioLabel'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clRed
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Notes: TDBMemo
        Left = 8
        Top = 80
        Width = 417
        Height = 145
        DataField = 'Notes'
        TabOrder = 0
      end
    end
    object CropCostPage: TTabSheet
      Caption = 'Costs'
      ImageIndex = 1
      OnShow = CropCostPageShow
      object Label2: TLabel
        Left = 8
        Top = 24
        Width = 285
        Height = 15
        Caption = 'Costs ($/ha) = Operation Cost + Product Cost * Rate'
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlue
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object Label22: TLabel
        Left = 8
        Top = 472
        Width = 347
        Height = 15
        Caption = 'Total costs($/ha) = Fixed costs + Nitrogen costs + Sowing costs'
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlue
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object CostGrid: TAdvColumnGrid
        Left = 8
        Top = 40
        Width = 473
        Height = 401
        Cursor = crDefault
        ColCount = 4
        DefaultRowHeight = 18
        DefaultDrawing = False
        FixedCols = 0
        RowCount = 40
        FixedRows = 1
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        GridLineWidth = 1
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goDrawFocusSelected]
        ParentFont = False
        PopupMenu = CostsPopup
        TabOrder = 0
        GridLineColor = clSilver
        ActiveCellShow = False
        ActiveCellFont.Charset = DEFAULT_CHARSET
        ActiveCellFont.Color = clWindowText
        ActiveCellFont.Height = -11
        ActiveCellFont.Name = 'MS Sans Serif'
        ActiveCellFont.Style = [fsBold]
        ActiveCellColor = clGray
        Bands.PrimaryColor = clInfoBk
        Bands.PrimaryLength = 1
        Bands.SecondaryColor = clWindow
        Bands.SecondaryLength = 1
        Bands.Print = False
        AutoNumAlign = False
        AutoSize = False
        VAlignment = vtaTop
        EnhTextSize = False
        EnhRowColMove = False
        SizeWithForm = False
        Multilinecells = True
        OnCellsChanged = CostGridCellsChanged
        OnGetEditorType = CostGridGetEditorType
        DragDropSettings.OleAcceptFiles = True
        DragDropSettings.OleAcceptText = True
        SortSettings.AutoColumnMerge = False
        SortSettings.Column = 0
        SortSettings.Show = False
        SortSettings.IndexShow = False
        SortSettings.IndexColor = clYellow
        SortSettings.Full = True
        SortSettings.SingleColumn = False
        SortSettings.IgnoreBlanks = False
        SortSettings.BlankPos = blFirst
        SortSettings.AutoFormat = True
        SortSettings.Direction = sdAscending
        SortSettings.FixedCols = False
        SortSettings.NormalCellsOnly = False
        SortSettings.Row = 0
        FloatingFooter.Color = clBtnFace
        FloatingFooter.Column = 0
        FloatingFooter.FooterStyle = fsFixedLastRow
        FloatingFooter.Visible = False
        ControlLook.Color = clBlack
        ControlLook.CheckSize = 15
        ControlLook.RadioSize = 10
        ControlLook.ControlStyle = csClassic
        ControlLook.FlatButton = False
        EnableBlink = False
        EnableHTML = True
        EnableWheel = True
        Flat = False
        HintColor = clInfoBk
        SelectionColor = clHighlight
        SelectionTextColor = clHighlightText
        SelectionRectangle = False
        SelectionResizer = False
        SelectionRTFKeep = False
        HintShowCells = False
        HintShowLargeText = False
        HintShowSizing = False
        PrintSettings.FooterSize = 0
        PrintSettings.HeaderSize = 0
        PrintSettings.Time = ppNone
        PrintSettings.Date = ppNone
        PrintSettings.DateFormat = 'dd/mm/yyyy'
        PrintSettings.PageNr = ppNone
        PrintSettings.Title = ppNone
        PrintSettings.Font.Charset = DEFAULT_CHARSET
        PrintSettings.Font.Color = clWindowText
        PrintSettings.Font.Height = -11
        PrintSettings.Font.Name = 'MS Sans Serif'
        PrintSettings.Font.Style = []
        PrintSettings.HeaderFont.Charset = DEFAULT_CHARSET
        PrintSettings.HeaderFont.Color = clWindowText
        PrintSettings.HeaderFont.Height = -11
        PrintSettings.HeaderFont.Name = 'MS Sans Serif'
        PrintSettings.HeaderFont.Style = []
        PrintSettings.FooterFont.Charset = DEFAULT_CHARSET
        PrintSettings.FooterFont.Color = clWindowText
        PrintSettings.FooterFont.Height = -11
        PrintSettings.FooterFont.Name = 'MS Sans Serif'
        PrintSettings.FooterFont.Style = []
        PrintSettings.Borders = pbNoborder
        PrintSettings.BorderStyle = psSolid
        PrintSettings.Centered = False
        PrintSettings.RepeatFixedRows = False
        PrintSettings.RepeatFixedCols = False
        PrintSettings.LeftSize = 0
        PrintSettings.RightSize = 0
        PrintSettings.ColumnSpacing = 0
        PrintSettings.RowSpacing = 0
        PrintSettings.TitleSpacing = 0
        PrintSettings.Orientation = poPortrait
        PrintSettings.PageNumberOffset = 0
        PrintSettings.MaxPagesOffset = 0
        PrintSettings.FixedWidth = 0
        PrintSettings.FixedHeight = 0
        PrintSettings.UseFixedHeight = False
        PrintSettings.UseFixedWidth = False
        PrintSettings.FitToPage = fpNever
        PrintSettings.PageNumSep = '/'
        PrintSettings.NoAutoSize = False
        PrintSettings.NoAutoSizeRow = False
        PrintSettings.PrintGraphics = False
        HTMLSettings.Width = 100
        HTMLSettings.XHTML = False
        Navigation.AdvanceDirection = adLeftRight
        Navigation.AllowClipboardShortCuts = True
        Navigation.AllowClipboardAlways = True
        Navigation.InsertPosition = pInsertBefore
        Navigation.HomeEndKey = heFirstLastColumn
        Navigation.TabToNextAtEnd = False
        Navigation.TabAdvanceDirection = adLeftRight
        ColumnSize.Location = clRegistry
        CellNode.Color = clSilver
        CellNode.NodeColor = clBlack
        CellNode.ShowTree = False
        MaxEditLength = 0
        MouseActions.DirectEdit = True
        IntelliPan = ipVertical
        URLColor = clBlue
        URLShow = False
        URLFull = False
        URLEdit = False
        ScrollType = ssNormal
        ScrollColor = clNone
        ScrollWidth = 17
        ScrollSynch = False
        ScrollProportional = False
        ScrollHints = shNone
        OemConvert = False
        FixedFooters = 0
        FixedRightCols = 0
        FixedColWidth = 137
        FixedRowHeight = 18
        FixedFont.Charset = DEFAULT_CHARSET
        FixedFont.Color = clWindowText
        FixedFont.Height = -11
        FixedFont.Name = 'Tahoma'
        FixedFont.Style = []
        FixedAsButtons = False
        FloatFormat = '%.2f'
        IntegralHeight = False
        WordWrap = False
        ColumnHeaders.Strings = (
          'Operation'
          'Operation cost'#13'($/ha)'#13
          'Product cost'#13'($/unit)'#13
          'Rate'#13'(unit/ha)')
        Lookup = False
        LookupCaseSensitive = False
        LookupHistory = False
        BackGround.Top = 0
        BackGround.Left = 0
        BackGround.Display = bdTile
        BackGround.Cells = bcNormal
        Filter = <>
        Columns = <
          item
            AutoMinSize = 0
            AutoMaxSize = 0
            Alignment = taLeftJustify
            Borders = []
            BorderPen.Color = clSilver
            CheckFalse = 'N'
            CheckTrue = 'Y'
            Color = clWindow
            ColumnPopupType = cpFixedCellsRClick
            EditLength = 0
            Editor = edComboEdit
            FilterCaseSensitive = False
            Fixed = False
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            Header = 'Operation'
            MinSize = 0
            MaxSize = 0
            Password = False
            PrintBorders = []
            PrintColor = clWhite
            PrintFont.Charset = DEFAULT_CHARSET
            PrintFont.Color = clWindowText
            PrintFont.Height = -11
            PrintFont.Name = 'Tahoma'
            PrintFont.Style = []
            ReadOnly = False
            ShowBands = False
            SortStyle = ssAutomatic
            SpinMax = 0
            SpinMin = 0
            SpinStep = 1
            Tag = 0
            Width = 137
          end
          item
            AutoMinSize = 0
            AutoMaxSize = 0
            Alignment = taRightJustify
            Borders = []
            BorderPen.Color = clSilver
            CheckFalse = 'N'
            CheckTrue = 'Y'
            Color = clWindow
            ColumnPopupType = cpFixedCellsRClick
            EditLength = 0
            Editor = edNormal
            FilterCaseSensitive = False
            Fixed = False
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            Header = 'Operation cost'#13'($/ha)'#13
            MinSize = 0
            MaxSize = 0
            Password = False
            PrintBorders = []
            PrintColor = clWhite
            PrintFont.Charset = DEFAULT_CHARSET
            PrintFont.Color = clWindowText
            PrintFont.Height = -11
            PrintFont.Name = 'Tahoma'
            PrintFont.Style = []
            ReadOnly = False
            ShowBands = False
            SortStyle = ssAutomatic
            SpinMax = 0
            SpinMin = 0
            SpinStep = 1
            Tag = 0
            Width = 82
          end
          item
            AutoMinSize = 0
            AutoMaxSize = 0
            Alignment = taRightJustify
            Borders = []
            BorderPen.Color = clSilver
            CheckFalse = 'N'
            CheckTrue = 'Y'
            Color = clWindow
            ColumnPopupType = cpFixedCellsRClick
            ComboItems.Strings = (
              'kg'
              'g'
              'tonne'
              'l')
            EditLength = 0
            Editor = edUnitEditBtn
            FilterCaseSensitive = False
            Fixed = False
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            Header = 'Product cost'#13'($/unit)'#13
            MinSize = 0
            MaxSize = 0
            Password = False
            PrintBorders = []
            PrintColor = clWhite
            PrintFont.Charset = DEFAULT_CHARSET
            PrintFont.Color = clWindowText
            PrintFont.Height = -11
            PrintFont.Name = 'Tahoma'
            PrintFont.Style = []
            ReadOnly = False
            ShowBands = False
            SortStyle = ssAutomatic
            SpinMax = 0
            SpinMin = 0
            SpinStep = 1
            Tag = 0
            Width = 114
          end
          item
            AutoMinSize = 0
            AutoMaxSize = 0
            Alignment = taRightJustify
            Borders = []
            BorderPen.Color = clSilver
            CheckFalse = 'N'
            CheckTrue = 'Y'
            Color = clWindow
            ColumnPopupType = cpFixedCellsRClick
            ComboItems.Strings = (
              'density from sim.'
              'nit. from sim.'
              'yield from sim.')
            EditLength = 0
            Editor = edComboEdit
            FilterCaseSensitive = False
            Fixed = False
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            Header = 'Rate'#13'(unit/ha)'
            MinSize = 0
            MaxSize = 0
            Password = False
            PrintBorders = []
            PrintColor = clWhite
            PrintFont.Charset = DEFAULT_CHARSET
            PrintFont.Color = clWindowText
            PrintFont.Height = -11
            PrintFont.Name = 'Tahoma'
            PrintFont.Style = []
            ReadOnly = False
            ShowBands = False
            SortStyle = ssAutomatic
            SpinMax = 0
            SpinMin = 0
            SpinStep = 1
            Tag = 0
            Width = 119
          end>
        FilterDropDown.Color = clWindow
        FilterDropDown.ColumnWidth = False
        FilterDropDown.Font.Charset = DEFAULT_CHARSET
        FilterDropDown.Font.Color = clWindowText
        FilterDropDown.Font.Height = -11
        FilterDropDown.Font.Name = 'MS Sans Serif'
        FilterDropDown.Font.Style = []
        FilterDropDown.Height = 200
        FilterDropDown.Width = 200
        ColWidths = (
          137
          82
          114
          119)
        RowHeights = (
          32
          18
          18
          18
          18
          18
          18
          18
          18
          18
          18
          18
          18
          18
          18
          18
          18
          18
          18
          18
          18
          18
          18
          18
          18
          18
          18
          18
          18
          18
          18
          18
          18
          18
          18
          18
          18
          18
          18
          18)
      end
    end
    object CropPricePage: TTabSheet
      Caption = 'Price'
      ImageIndex = 2
      OnShow = CropPricePageShow
      object PriceLabel: TLabel
        Left = 208
        Top = 40
        Width = 90
        Height = 15
        Caption = 'for %100 of yield'
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object Label3: TLabel
        Left = 24
        Top = 232
        Width = 307
        Height = 15
        Caption = 'Wet yield ($/ta) = DryYield / (1 - Moisture content as prop)'
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlue
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object Label4: TLabel
        Left = 24
        Top = 40
        Width = 31
        Height = 15
        Caption = 'Price:'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object Label5: TLabel
        Left = 24
        Top = 72
        Width = 67
        Height = 15
        Caption = 'Downgrade:'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object Label6: TLabel
        Left = 296
        Top = 72
        Width = 38
        Height = 15
        Caption = 'of yield'
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object Label7: TLabel
        Left = 24
        Top = 104
        Width = 72
        Height = 15
        Caption = 'Harvest loss:'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object Label8: TLabel
        Left = 208
        Top = 72
        Width = 14
        Height = 15
        Caption = 'for'
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object Label9: TLabel
        Left = 24
        Top = 136
        Width = 93
        Height = 15
        Caption = 'Moisture content:'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object Label10: TLabel
        Left = 24
        Top = 256
        Width = 331
        Height = 15
        Caption = 'Harvested yield ($/ha) = Wet yield * (1 - Harvest loss as prop)'
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlue
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object Label11: TLabel
        Left = 24
        Top = 280
        Width = 336
        Height = 15
        Caption = 'Return ($/ha) = Price * (1 - Downgrade prop) * Harvested yield'
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlue
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object Label13: TLabel
        Left = 98
        Top = 304
        Width = 302
        Height = 15
        Caption = '+ Downgrade price * Downgrade prop * Harvested yield'
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlue
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object Label14: TLabel
        Left = 124
        Top = 40
        Width = 7
        Height = 15
        Caption = '$'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object Label15: TLabel
        Left = 124
        Top = 72
        Width = 7
        Height = 15
        Caption = '$'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object Label16: TLabel
        Left = 120
        Top = 104
        Width = 11
        Height = 15
        Caption = '%'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object Label17: TLabel
        Left = 120
        Top = 136
        Width = 11
        Height = 15
        Caption = '%'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object Label18: TLabel
        Left = 224
        Top = 72
        Width = 11
        Height = 15
        Caption = '%'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object PriceEdit: TAdvEdit
        Left = 136
        Top = 36
        Width = 57
        Height = 21
        AutoFocus = False
        EditAlign = eaLeft
        EditType = etString
        ErrorMarkerPos = 0
        ErrorMarkerLen = 0
        ErrorColor = clRed
        ErrorFontColor = clWhite
        ExcelStyleDecimalSeparator = False
        Flat = True
        FlatLineColor = clBlack
        FlatParentColor = True
        FocusAlign = eaDefault
        FocusBorder = False
        FocusColor = clBtnFace
        FocusFontColor = clWindowText
        FocusLabel = False
        FocusWidthInc = 0
        ModifiedColor = clHighlight
        DisabledColor = clSilver
        URLColor = clBlue
        ReturnIsTab = False
        LengthLimit = 0
        TabOnFullLength = False
        Precision = 0
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
        BorderStyle = bsNone
        Color = clBtnFace
        Enabled = True
        HintShowLargeText = False
        OleDropTarget = False
        OleDropSource = False
        Signed = False
        TabOrder = 0
        Transparent = False
        Visible = True
        OnChange = PriceEditBoxesChanged
      end
      object DowngradeReturnEdit: TAdvEdit
        Left = 136
        Top = 68
        Width = 57
        Height = 21
        AutoFocus = False
        EditAlign = eaLeft
        EditType = etString
        ErrorMarkerPos = 0
        ErrorMarkerLen = 0
        ErrorColor = clRed
        ErrorFontColor = clWhite
        ExcelStyleDecimalSeparator = False
        Flat = True
        FlatLineColor = clBlack
        FlatParentColor = True
        FocusAlign = eaDefault
        FocusBorder = False
        FocusColor = clBtnFace
        FocusFontColor = clWindowText
        FocusLabel = False
        FocusWidthInc = 0
        ModifiedColor = clHighlight
        DisabledColor = clSilver
        URLColor = clBlue
        ReturnIsTab = False
        LengthLimit = 0
        TabOnFullLength = False
        Precision = 0
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
        BorderStyle = bsNone
        Color = clBtnFace
        Enabled = True
        HintShowLargeText = False
        OleDropTarget = False
        OleDropSource = False
        Signed = False
        TabOrder = 1
        Transparent = False
        Visible = True
        OnChange = PriceEditBoxesChanged
      end
      object HarvestLossEdit: TAdvEdit
        Left = 136
        Top = 100
        Width = 57
        Height = 21
        AutoFocus = False
        EditAlign = eaLeft
        EditType = etString
        ErrorMarkerPos = 0
        ErrorMarkerLen = 0
        ErrorColor = clRed
        ErrorFontColor = clWhite
        ExcelStyleDecimalSeparator = False
        Flat = True
        FlatLineColor = clBlack
        FlatParentColor = True
        FocusAlign = eaDefault
        FocusBorder = False
        FocusColor = clBtnFace
        FocusFontColor = clWindowText
        FocusLabel = False
        FocusWidthInc = 0
        ModifiedColor = clHighlight
        DisabledColor = clSilver
        URLColor = clBlue
        ReturnIsTab = False
        LengthLimit = 0
        TabOnFullLength = False
        Precision = 0
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
        BorderStyle = bsNone
        Color = clBtnFace
        Enabled = True
        HintShowLargeText = False
        OleDropTarget = False
        OleDropSource = False
        Signed = False
        TabOrder = 2
        Transparent = False
        Visible = True
        OnChange = PriceEditBoxesChanged
      end
      object MoistureContentEdit: TAdvEdit
        Left = 136
        Top = 132
        Width = 57
        Height = 21
        AutoFocus = False
        EditAlign = eaLeft
        EditType = etString
        ErrorMarkerPos = 0
        ErrorMarkerLen = 0
        ErrorColor = clRed
        ErrorFontColor = clWhite
        ExcelStyleDecimalSeparator = False
        Flat = True
        FlatLineColor = clBlack
        FlatParentColor = True
        FocusAlign = eaDefault
        FocusBorder = False
        FocusColor = clBtnFace
        FocusFontColor = clWindowText
        FocusLabel = False
        FocusWidthInc = 0
        ModifiedColor = clHighlight
        DisabledColor = clSilver
        URLColor = clBlue
        ReturnIsTab = False
        LengthLimit = 0
        TabOnFullLength = False
        Precision = 0
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
        BorderStyle = bsNone
        Color = clBtnFace
        Enabled = True
        HintShowLargeText = False
        OleDropTarget = False
        OleDropSource = False
        Signed = False
        TabOrder = 3
        Transparent = False
        Visible = True
        OnChange = PriceEditBoxesChanged
      end
      object DowngradePercentEdit: TAdvEdit
        Left = 237
        Top = 68
        Width = 57
        Height = 21
        AutoFocus = False
        EditAlign = eaLeft
        EditType = etString
        ErrorMarkerPos = 0
        ErrorMarkerLen = 0
        ErrorColor = clRed
        ErrorFontColor = clWhite
        ExcelStyleDecimalSeparator = False
        Flat = True
        FlatLineColor = clBlack
        FlatParentColor = True
        FocusAlign = eaDefault
        FocusBorder = False
        FocusColor = clBtnFace
        FocusFontColor = clWindowText
        FocusLabel = False
        FocusWidthInc = 0
        ModifiedColor = clHighlight
        DisabledColor = clSilver
        URLColor = clBlue
        ReturnIsTab = False
        LengthLimit = 0
        TabOnFullLength = False
        Precision = 0
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
        BorderStyle = bsNone
        Color = clBtnFace
        Enabled = True
        HintShowLargeText = False
        OleDropTarget = False
        OleDropSource = False
        Signed = False
        TabOrder = 4
        Transparent = False
        Visible = True
        OnChange = DowngradePercentEditChange
      end
    end
  end
  object ScenarioTree: TTreeView
    Left = 11
    Top = 56
    Width = 169
    Height = 481
    BevelInner = bvNone
    BevelKind = bkSoft
    BorderStyle = bsNone
    HideSelection = False
    Indent = 19
    TabOrder = 1
    OnChange = ScenarioTreeChange
    OnChanging = ScenarioTreeChanging
    OnEdited = ScenarioTreeEdited
    OnEditing = ScenarioTreeEditing
  end
  object CoolBar1: TCoolBar
    Left = 0
    Top = 0
    Width = 691
    Height = 30
    AutoSize = True
    BandBorderStyle = bsNone
    Bands = <
      item
        Control = ToolBar1
        ImageIndex = -1
        MinHeight = 30
        Width = 691
      end>
    EdgeBorders = []
    EdgeInner = esNone
    EdgeOuter = esNone
    object ToolBar1: TToolBar
      Left = 9
      Top = 0
      Width = 678
      Height = 30
      ButtonHeight = 24
      ButtonWidth = 96
      EdgeBorders = []
      EdgeInner = esNone
      EdgeOuter = esNone
      Flat = True
      Images = ImageList1
      List = True
      ParentShowHint = False
      ShowCaptions = True
      ShowHint = True
      TabOrder = 0
      Wrapable = False
      object NewButton: TToolButton
        Left = 0
        Top = 0
        Hint = 'Create a new economics database'
        AutoSize = True
        Caption = '&New'
        ImageIndex = 8
        OnClick = NewButtonClick
      end
      object OpenButton: TToolButton
        Left = 55
        Top = 0
        Hint = 'Open a different economics database'
        AutoSize = True
        Caption = '&Open'
        ImageIndex = 0
        OnClick = OpenButtonClick
      end
      object ToolButton2: TToolButton
        Left = 114
        Top = 0
        Width = 8
        Caption = 'ToolButton2'
        ImageIndex = 1
        Style = tbsSeparator
      end
      object AddButton: TToolButton
        Left = 122
        Top = 0
        AutoSize = True
        Caption = '&Add'
        DropdownMenu = AddPopup
        ImageIndex = 1
        Style = tbsDropDown
      end
      object DeleteButton: TToolButton
        Left = 187
        Top = 0
        AutoSize = True
        Caption = '&Delete'
        ImageIndex = 2
        OnClick = deleteSelected
      end
      object RenameButton: TToolButton
        Left = 251
        Top = 0
        AutoSize = True
        Caption = '&Rename'
        ImageIndex = 3
        OnClick = renameSelected
      end
      object ToolButton7: TToolButton
        Left = 324
        Top = 0
        Width = 8
        Caption = 'ToolButton7'
        ImageIndex = 7
        Style = tbsSeparator
      end
      object CopyButton: TToolButton
        Left = 332
        Top = 0
        AutoSize = True
        Caption = '&Copy'
        ImageIndex = 4
        OnClick = copySelected
      end
      object PasteButton: TToolButton
        Left = 389
        Top = 0
        AutoSize = True
        Caption = '&Paste'
        ImageIndex = 5
        OnClick = pasteSelected
      end
      object ToolButton3: TToolButton
        Left = 449
        Top = 0
        Width = 8
        Caption = 'ToolButton3'
        ImageIndex = 4
        Style = tbsSeparator
      end
      object SeedWeightButton: TToolButton
        Left = 457
        Top = 0
        Hint = 'Edit the seed weights for various crops'
        AutoSize = True
        Caption = '&Seed weight'
        ImageIndex = 7
        OnClick = SeedWeightButtonClick
      end
      object PrimeHardButton: TToolButton
        Left = 549
        Top = 0
        Hint = 'Edit the wheat protein increments'
        AutoSize = True
        Caption = 'Wheat protein'
        ImageIndex = 6
        OnClick = WheatProteinButtonClick
      end
    end
  end
  object OkButton: TButton
    Left = 528
    Top = 544
    Width = 75
    Height = 25
    Caption = '&Ok'
    ModalResult = 1
    TabOrder = 3
  end
  object CancelButton: TButton
    Left = 608
    Top = 544
    Width = 75
    Height = 25
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object ImageList1: TImageList
    Height = 18
    Width = 18
    Left = 48
    Top = 96
    Bitmap = {
      494C010109000E00040012001200FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000480000004800000001002000000000000051
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008080
      8000808080008080800080808000808080008080800080808000808080008080
      8000808080008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008080
      8000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008080
      8000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008080
      8000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008080
      8000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008080
      8000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008080
      8000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008080
      8000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008080
      8000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008080
      8000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00808080008080
      8000808080008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008080
      8000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0080808000FFFF
      FF00808080000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008080
      8000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00808080008080
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008080
      8000808080008080800080808000808080008080800080808000808080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C0C0C0008080800080808000808080008080800080808000808080008080
      8000808080008080800080808000808000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C0C0C00080808000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000C0C0C000C0C0C00080808000808000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000808000008080
      0000808000008080000080800000808000008080000080800000808000008080
      0000000000000000000080808000808080008080800080808000808080008080
      8000808080008080800080808000808080008080800080808000808080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C0C0
      C000C0C0C00080808000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00C0C0C00080808000808000000000000000000000000000000000
      000000000000808080008080800080808000808080008080800080800000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF008080
      00000000000080808000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF008080
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000008080800000000000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000000000000C0C0C000C0C0
      C000C0C0C00080808000FFFFFF00C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000C0C0C000C0C0C00080808000808000000000000000000000000000000000
      00008080800000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0080800000FFFF
      FF00000000000000000000000000000000000000000000000000FFFFFF008080
      00000000000080808000FFFFFF0080808000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF008080
      8000000000000000000000000000000000000000000000000000000000000000
      0000808080008080800080808000000000008080800080808000808080000000
      0000000000000000000000000000000000000000000000000000C0C0C000FFFF
      FF00FFFFFF0080808000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00C0C0C00080808000808000000000000000000000000000000000
      00008080800000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0080800000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF008080
      00000000000080808000FFFFFF0080808000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF008080
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000008080800000000000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000000000000C0C0C000FFFF
      FF00FFFFFF0080808000FFFFFF00C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000C0C0C000C0C0C00080808000808000000000000000000000000000000000
      00008080800000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0080800000FFFF
      FF00000000000000000000000000FFFFFF008080000080800000808000008080
      00000000000080808000FFFFFF0080808000FFFFFF008080800080808000FFFF
      FF00FFFFFF0080808000FFFFFF00FFFFFF00FFFFFF0080808000FFFFFF008080
      8000000000000000000000000000000000000000000000000000000000008080
      8000000000000000000000000000808080000000000000000000000000008080
      8000000000000000000000000000000000000000000000000000C0C0C000FFFF
      FF00C0C0C00080808000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00C0C0C00080808000808000000000000000000000000000000000
      00008080800000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0080800000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0080800000FFFFFF00808000000000
      00000000000080808000FFFFFF008080800080808000FFFFFF00FFFFFF008080
      8000FFFFFF0080808000FFFFFF00FFFFFF00FFFFFF0080808000FFFFFF008080
      8000000000000000000000000000000000000000000000000000808080008080
      8000808080000000000080808000808080008080800000000000808080008080
      8000808080000000000000000000000000000000000000000000C0C0C000FFFF
      FF00FFFFFF0080808000FFFFFF00C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000C0C0C000C0C0C00080808000808000000000000000000000000000000000
      00008080800000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0080800000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF008080000080800000000000000000
      00000000000080808000FFFFFF0080808000FFFFFF00FFFFFF00FFFFFF008080
      8000FFFFFF0080808000FFFFFF00FFFFFF00FFFFFF0080808000FFFFFF008080
      8000000000000000000000000000000000000000000000000000000000008080
      8000000000000000000000000000808080000000000000000000000000008080
      8000000000000000000000000000000000000000000000000000C0C0C000FFFF
      FF00C0C0C00080808000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00C0C0C00080808000808000000000000000000000000000000000
      00008080800000FFFF0000FFFF0000FFFF0000FFFF0000FFFF00808000008080
      0000808000008080000080800000808000008080000080808000000000000000
      00000000000080808000FFFFFF0080808000FFFFFF00FFFFFF00FFFFFF008080
      8000FFFFFF0080808000FFFFFF00FFFFFF00FFFFFF0080808000FFFFFF008080
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000008080800000000000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000000000000C0C0C000FFFF
      FF00FFFFFF0080808000FFFFFF00C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000C0C0C000C0C0C00080808000808080000000000000000000000000000000
      00008080800000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0080808000000000000000
      00000000000080808000FFFFFF008080800080808000FFFFFF00FFFFFF008080
      8000FFFFFF008080800080808000FFFFFF00FFFFFF0080808000FFFFFF008080
      8000000000000000000000000000000000000000000000000000000000000000
      0000808080008080800080808000000000008080800080808000808080000000
      0000000000000000000000000000000000000000000000000000C0C0C000FFFF
      FF00C0C0C00080808000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00C0C0
      C000808080008080800080800000000000000000000000000000000000000000
      00008080800000FFFF0000FFFF00808080008080800080808000808080008080
      800080808000808080008080800000FFFF0000FFFF0080808000000000000000
      00000000000080808000FFFFFF0080808000FFFFFF008080800080808000FFFF
      FF00FFFFFF0080808000FFFFFF008080800080808000FFFFFF00FFFFFF008080
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000008080800000000000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000000000000C0C0C000FFFF
      FF00FFFFFF0080808000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00C0C0
      C000808080008080000000000000000000000000000000000000000000000000
      00008080800000FFFF0000FFFF0080808000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF008080800000FFFF0000FFFF0080808000000000000000
      00000000000080808000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF0080808000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF008080
      8000000000000000000000000000000000000000000000000000000000008080
      8000000000000000000000000000808080000000000000000000000000008080
      8000000000000000000000000000000000000000000000000000C0C0C000FFFF
      FF00C0C0C0008080800080808000808080008080800080808000808080008080
      8000808000000000000000000000000000000000000000000000000000000000
      00008080800000FFFF0000FFFF0000FFFF0080808000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF008080800000FFFF0000FFFF0000FFFF0080808000000000000000
      00000000000080808000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF0080808000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF008080
      8000000000000000000000000000000000000000000000000000808080008080
      8000808080000000000080808000808080008080800000000000808080008080
      8000808080000000000000000000000000000000000000000000C0C0C000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00C0C0C00080808000808080008080
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008080800080808000808080008080800080808000FFFFFF00FFFF
      FF00808080008080800080808000808080008080800000000000000000000000
      00000000000080808000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF008080
      8000000000000000000000000000000000000000000000000000000000008080
      8000000000000000000000000000808080000000000000000000000000008080
      8000000000000000000000000000000000000000000000000000C0C0C000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00C0C0C00080808000808000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000808080008080
      8000808080000000000000000000000000000000000000000000000000000000
      0000000000000000000080808000808080008080800080808000808080008080
      8000808080008080800080808000808080008080800080808000808080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C0C0C000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C00080800000800000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000C0C0C0008080800080808000808080008080
      8000808080008080800080808000000000000000000080808000808080008080
      8000808080008080800080808000808080008080800080808000808080008080
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000C0C0C000808080008080800080808000C0C0
      C000000000000000000000000000000000000000000000000000C0C0C0008080
      8000808080000000000000000000000000000000000000000000000000000000
      000000000000000000000000000080808000C0C0C00080808000808080008080
      8000C0C0C0008080800080808000000000000000000080808000808080008080
      0000808000008080000080800000808000008080000080800000808000008080
      0000808080000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00008080800080808000C0C0C000000000000000000000000000000000000000
      0000000000000000000000000000808080000000FF000000800080808000C0C0
      C0000000000000000000000000000000000000000000C0C0C000808080000000
      FF00808080000000000000000000000000000000000000000000000000000000
      00000000000000000000000000008080800080808000C0C0C000C0C0C0008080
      80008080800080808000C0C0C00000000000000000008080800000FFFF008080
      8000808000008080000080800000808000008080000080800000808000008080
      0000808000008080800000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000000FF
      000000FF000080808000C0C0C000000000000000000000000000000000000000
      0000000000000000000000000000C0C0C0000000FF000000FF00808080008080
      8000C0C0C000000000000000000000000000C0C0C000800080000000FF008080
      8000C0C0C000000000000000000000000000C0C0C000C0C0C000C0C0C000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C00080808000C0C0C0008080
      000080808000C0C0C000C0C0C00000000000000000008080800000FFFF0000FF
      FF00808080008080000080800000808000008080000080800000808000008080
      0000808000008080000080808000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000000FF
      000000FF000080808000C0C0C000000000000000000000000000000000000000
      000000000000000000000000000000000000808080000000FF000000FF008080
      800080808000C0C0C00000000000C0C0C0000000FF000000FF0080808000C0C0
      C000000000000000000000000000000000008080800080808000808080008080
      8000808080008080800080808000808080008080800080808000C0C0C0000000
      000080808000808080008080800000000000000000008080800000FFFF0000FF
      FF0000FFFF008080800080800000808000008080000080800000808000008080
      0000808000008080000080800000808080000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000000FF
      000000FF000080808000C0C0C000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000808080000000FF008000
      80008080800080808000C0C0C0000000FF000000FF0080808000C0C0C0000000
      000000000000000000000000000080808000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0080808000C0C0C0008080
      8000FFFFFF00808080008080800000000000000000008080800000FFFF0000FF
      FF0000FFFF0000FFFF0080808000808080008080800080808000808080008080
      8000808080008080800080808000808080000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000000FF
      000000FF000080808000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000000000000000000000000000000000000000000000000000808080000000
      FF0080008000808080000000FF000000FF0080808000C0C0C000000000000000
      000000000000000000000000000080808000FFFFFF0080000000800000008000
      000080000000800000008000000080000000FFFFFF0080808000C0C0C0008080
      8000FFFFFF00808080008080800000000000000000008080800000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF008080
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000000FF
      000000FF0000808080008080800080808000808080008080800080808000C0C0
      C000000000000000000000000000000000000000000000000000000000008080
      80000000FF000000FF000000FF0080808000C0C0C00000000000000000000000
      000000000000000000000000000080808000FFFFFF0080000000800000008000
      000080000000800000008000000080000000FFFFFF0080808000C0C0C0008080
      8000FFFFFF00808080008080800000000000000000008080800000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF008080
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000FF000000FF000000FF000000FF000000FF000000FF
      000000FF000000FF000000FF000000FF000000FF000000FF0000808080000000
      000000000000000000000000000000000000000000000000000000000000C0C0
      C0000000FF000000FF00800080008080800080808000C0C0C000C0C0C0000000
      000000000000000000000000000080808000FFFFFF0080000000800000008000
      000080000000800000008000000080000000FFFFFF0080808000C0C0C0008080
      8000FFFFFF00808080008080800000000000000000008080800000FFFF0000FF
      FF0000FFFF008080800080808000808080008080800080808000808080008080
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000FF000000FF000000FF000000FF000000FF000000FF
      000000FF000000FF000000FF000000FF000000FF000000FF0000000000000000
      0000000000000000000000000000000000000000000000000000C0C0C0000000
      FF000000FF00808080000000FF00800080008080800080808000C0C0C000C0C0
      C00000000000000000000000000080808000FFFFFF0080000000800000008000
      000080000000800000008000000080000000FFFFFF0080808000C0C0C0008080
      8000FFFFFF008080800080808000000000000000000000000000808080008080
      8000808080000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000000FF
      000000FF000080808000C0C0C000000000000000000000000000000000000000
      000000000000000000000000000000000000C0C0C000C0C0C0000000FF000000
      FF0080808000C0C0C000C0C0C0000000FF000000FF0080808000808080008080
      8000C0C0C000000000000000000080808000FFFFFF0080000000800000008000
      000080000000800000008000000080000000FFFFFF0080808000C0C0C0008080
      8000FFFFFF008080800080808000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000000FF
      000000FF000080808000C0C0C000000000000000000000000000000000000000
      000000000000000000000000000000000000C0C0C0000000FF000000FF008080
      8000C0C0C0000000000000000000C0C0C0000000FF000000FF00808080008080
      8000C0C0C000000000000000000080808000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0080808000C0C0C0008080
      8000FFFFFF008080800080808000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000000FF
      000000FF000080808000C0C0C000000000000000000000000000000000000000
      0000000000000000000000000000C0C0C0000000FF000000FF0080008000C0C0
      C0000000000000000000000000000000000000000000808080000000FF00C0C0
      C000000000000000000000000000808080008080800080808000808080008080
      8000808080008080800080808000808080008080800080808000C0C0C0008080
      0000808080008080800080808000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000000FF
      000000FF000080808000C0C0C000000000000000000000000000000000000000
      00000000000000000000000000000000FF000000FF0080008000C0C0C0000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000C0C0C0008080800080808000FFFFFF008080
      8000808000008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000000FF
      000000FF00000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000FF000000FF00C0C0C000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000080808000FFFFFF00C0C0C00080808000C0C0
      C000FFFFFF008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000808080008080800080808000C0C0C0008080
      8000808080008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000048000000480000000100010000000000600300000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FFFFC0000000000000000000FFFFC0000000000000000000
      E003C0000000000000000000E003C0000000000000000000E003C00000000000
      00000000E003C0000000000000000000E003C0000000000000000000E003C000
      0000000000000000E003C0000000000000000000E003C0000000000000000000
      E003C0000000000000000000E003C0000000000000000000E007C00000000000
      00000000E00FC0000000000000000000E01FC0000000000000000000FFFFC000
      0000000000000000FFFFC0000000000000000000FFFFC0000000000000000000
      FFFFFFFFFFFFFFFFFF000000F000FFFFFFFFFFFFFF000000F000FFC00C001FFF
      FF000000E000F80008000FFBBF000000C000F00008000FF11F000000C000F000
      08000FFBBF000000C000F00008000FEEEF000000C000F00018000FC447000000
      C000F00038000FEEEF000000C000F00038000FFBBF000000C000F00038000FF1
      1F000000C001F00038000FFBBF000000C003F00038000FEEEF000000C007F000
      38000FC447000000C00FF80078000FEEEF000000C01FFF87FC001FFFFF000000
      C01FFFFFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFFFFFF
      FF000000FFFFFFFFFFFFFFFE01000000800FFFFFFE0FC7FE010000008007FFF1
      FE0F87FE010000008003FFE1FE070700010000008001FFE1FF020F0001000000
      8000FFE1FF801E00010000008000FFE00FC03E0001000000800FFFE00FE07E00
      01000000800FFC001FE01E0001000000800FFC003FC00E0001000000C7F8FFE1
      FF00060001000000FFFCFFE1FF06060001000000FFBAFFE1FE0F8E0001000000
      FFC7FFE1FE1FFFFE03000000FFFFFFE7FE3FFFFE03000000FFFFFFFFFFFFFFFE
      03000000FFFFFFFFFFFFFFFFFF00000000000000000000000000000000000000
      000000000000}
  end
  object OpenDialog: TOpenDialog
    DefaultExt = 'mdb'
    Filter = 'Economics databases(*.mdb)|*.mdb'
    Title = 'Select an economics database'
    Left = 80
    Top = 96
  end
  object SaveDialog: TSaveDialog
    DefaultExt = 'mdb'
    Filter = 'Economics databases(*.mdb)|*.mdb'
    Title = 'Enter a new name and folder for economics database'
    Left = 112
    Top = 96
  end
  object AddPopup: TPopupMenu
    Left = 144
    Top = 96
    object AddScenario1: TMenuItem
      Caption = 'Add &Scenario'
      OnClick = addScenario
    end
    object AddCrop1: TMenuItem
      Caption = 'Add &Crop'
      OnClick = addCrop
    end
  end
  object CostsPopup: TPopupMenu
    Left = 352
    Top = 240
    object Deleterow1: TMenuItem
      Caption = '&Delete row'
      OnClick = Deleterow1Click
    end
  end
end
