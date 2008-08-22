object MDIChild: TMDIChild
  Left = 1625
  Top = 301
  Width = 671
  Height = 400
  Caption = 'Chart'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'System'
  Font.Style = []
  FormStyle = fsMDIChild
  Menu = MainMenu2
  OldCreateOrder = False
  Position = poDefault
  PrintScale = poPrintToFit
  Visible = True
  WindowState = wsMaximized
  OnActivate = FormActivate
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 16
  object Splitter: TSplitter
    Left = 209
    Top = 0
    Width = 7
    Height = 346
    Cursor = crHSplit
    Beveled = True
    Color = clMenu
    MinSize = 1
    ParentColor = False
    Visible = False
  end
  object Grid: TDBAdvStringGrid
    Left = 0
    Top = 0
    Width = 209
    Height = 346
    Cursor = crDefault
    Align = alLeft
    ColCount = 5
    DefaultRowHeight = 18
    DefaultDrawing = False
    FixedCols = 0
    RowCount = 2
    FixedRows = 1
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    GridLineWidth = 1
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goDrawFocusSelected, goColSizing]
    ParentFont = False
    TabOrder = 0
    Visible = False
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
    Multilinecells = False
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
    Navigation.InsertPosition = pInsertBefore
    Navigation.HomeEndKey = heFirstLastColumn
    Navigation.TabToNextAtEnd = False
    Navigation.TabAdvanceDirection = adLeftRight
    ColumnSize.Location = clRegistry
    CellNode.Color = clSilver
    CellNode.NodeColor = clBlack
    CellNode.ShowTree = False
    MaxEditLength = 0
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
    FixedColWidth = 64
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
    Lookup = False
    LookupCaseSensitive = False
    LookupHistory = False
    BackGround.Top = 0
    BackGround.Left = 0
    BackGround.Display = bdTile
    BackGround.Cells = bcNormal
    Filter = <>
    CloseWhenLoaded = False
    KeepLinked = True
    DataMapMode = dmRow
    DetailGrid = False
    Fields = <>
    PageMode = False
    ShowMemoFields = True
    ShowPictureFields = True
    ShowBooleanFields = False
    ShowIndicator = False
    ColWidths = (
      64
      64
      64
      64
      64)
    RowHeights = (
      18
      18)
  end
  object MainMenu2: TMainMenu
    Left = 280
    Top = 64
    object EditMenu: TMenuItem
      Caption = '&Edit'
      GroupIndex = 2
      object EditCopyMenu: TMenuItem
        Caption = '&Copy'
        OnClick = EditCopy
      end
      object EditCopyWithoutMenu: TMenuItem
        Caption = 'Copy &without chart background'
        OnClick = EditCopyWithout
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object EditSendDatatoEXCELMenu: TMenuItem
        Caption = 'Copy &data (EXCEL format)'
        GroupIndex = 2
        OnClick = SendDataToEXCEL
      end
      object CopyScenarioMenu: TMenuItem
        Caption = 'Copy scenario settings'
        GroupIndex = 2
        OnClick = CopyScenarioMenuClick
      end
    end
    object View1: TMenuItem
      Caption = '&View'
      GroupIndex = 2
      object ChartsViewDataMenu: TMenuItem
        Caption = '&Data'
        GroupIndex = 2
        OnClick = ViewData
      end
      object ChartsViewSettingsMenu: TMenuItem
        Caption = '&Simulation details'
        Checked = True
        GroupIndex = 2
        OnClick = ChartsViewSettingsMenuClick
      end
      object N5: TMenuItem
        Caption = '-'
        GroupIndex = 2
      end
      object Stats1: TMenuItem
        Caption = 'S&tats'
        GroupIndex = 2
        OnClick = Stats1Click
      end
    end
    object ChartsMenu: TMenuItem
      Caption = '&Chart'
      GroupIndex = 3
      object ChartsTimeSeriesMenu: TMenuItem
        Caption = '&Time series...'
        GroupIndex = 2
        OnClick = TimeSeriesChart
      end
      object ChartsDifferenceMenu: TMenuItem
        Caption = '&Difference...'
        GroupIndex = 2
        OnClick = DifferenceChart
      end
      object N6: TMenuItem
        Caption = '-'
        GroupIndex = 2
      end
      object ChartsPieMenu: TMenuItem
        Caption = '&Pie...'
        GroupIndex = 2
        OnClick = PieChart
      end
      object ChartsFrequencyMenu: TMenuItem
        Caption = '&Frequency...'
        GroupIndex = 2
        OnClick = FrequencyChart
      end
      object ChartsProbabilityMenu: TMenuItem
        Caption = 'P&robability...'
        GroupIndex = 2
        OnClick = ProbabilityChart
      end
      object N1: TMenuItem
        Caption = '-'
        GroupIndex = 2
      end
      object ChartsSummaryMenu: TMenuItem
        Caption = '&Summary...'
        GroupIndex = 2
        OnClick = SummaryTable
      end
      object ChartsXYMenu: TMenuItem
        Caption = '&XY...'
        GroupIndex = 2
        OnClick = XYChart
      end
      object ChartsNoChartMenu: TMenuItem
        Caption = '&No Chart'
        GroupIndex = 2
        OnClick = ChartsNoChartMenuClick
      end
      object N2: TMenuItem
        Caption = '-'
        GroupIndex = 2
      end
      object ChartsPropertiesMenu: TMenuItem
        Caption = 'Editor...'
        GroupIndex = 2
        OnClick = Properties
      end
    end
    object OptionsMenu: TMenuItem
      Caption = '&Options'
      GroupIndex = 3
      object OpionsSelectSimulationsMenu: TMenuItem
        Caption = '&Choose Scenarios...'
        GroupIndex = 2
        OnClick = SelectSimulations
      end
      object N3: TMenuItem
        Caption = '-'
        GroupIndex = 2
      end
      object OptionsPreferencesMenu: TMenuItem
        Caption = '&Preferences'
        GroupIndex = 3
        OnClick = OptionsPreferences
      end
    end
  end
  object Grid_data_source: TDataSource
    DataSet = APSTable_2_TDataSet
    Left = 432
    Top = 24
  end
  object APSTable_2_TDataSet: TAPSTable_2_TDataSet
    DesignActivation = True
    AttachedAutoRefresh = True
    AttachMaxCount = 1
    FieldDefs = <>
    IndexDefs = <>
    SortOptions = []
    AllDataOptions = [mtfSaveData, mtfSaveNonVisible, mtfSaveBlobs, mtfSaveFiltered, mtfSaveIgnoreRange, mtfSaveIgnoreMasterDetail, mtfSaveDeltas]
    CommaTextOptions = [mtfSaveData]
    CSVQuote = '"'
    CSVFieldDelimiter = ','
    CSVRecordDelimiter = ','
    CSVTrueString = 'True'
    CSVFalseString = 'False'
    PersistentSaveOptions = [mtfSaveData, mtfSaveNonVisible, mtfSaveIgnoreRange, mtfSaveIgnoreMasterDetail]
    PersistentBackup = False
    ProgressFlags = [mtpcLoad, mtpcSave, mtpcCopy]
    FilterOptions = []
    Version = '2.53g'
    LanguageID = 0
    SortID = 0
    SubLanguageID = 1
    LocaleID = 1024
    Left = 376
    Top = 24
  end
  object AllData: TAPSTable
    Left = 344
    Top = 24
  end
  object working: TAPSTable
    Left = 368
    Top = 136
  end
end
