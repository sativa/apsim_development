object FilterForm: TFilterForm
  Left = 247
  Top = 156
  Width = 544
  Height = 259
  BorderIcons = []
  Caption = 'Filter Values'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object BitBtn2: TBitBtn
    Left = 272
    Top = 200
    Width = 75
    Height = 25
    TabOrder = 0
    Kind = bkCancel
  end
  object BitBtn1: TBitBtn
    Left = 192
    Top = 200
    Width = 75
    Height = 25
    TabOrder = 1
    Kind = bkOK
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 536
    Height = 193
    ActivePage = Scenario1Sheet
    Align = alTop
    TabOrder = 2
    object Scenario1Sheet: TTabSheet
      Caption = 'Scenario1'
      object FilterBox1: TPSCFltBox
        Left = 0
        Top = 32
        Width = 524
        Height = 129
        PopupParams.ClientEdge = True
        DefStoreMode = smText
        KeyMapping = 'Default'
        Options = [fboSortFields, fboSortConditions, fboCanAdd, fboCanDelete, fboCanIndent, fboCanOpen, fboCanSave, fboCanSaveToMem, fboCanLoadFromMem]
        ForceSQLDateTime = False
        ForceSQLBooleans = False
        FilterDateTimeFormat.DateSeparator = #0
        FilterDateTimeFormat.TimeSeparator = #0
        FilterDateTimeFormat.AlwaysY2k = True
        SQLDateTimeFormat.DateSeparator = '/'
        SQLDateTimeFormat.TimeSeparator = ':'
        SQLDateTimeFormat.AMSymbol = 'AM'
        SQLDateTimeFormat.PMSymbol = 'PM'
        SQLDateTimeFormat.DateFormat = 'MM/DD/YYYY'
        SQLDateTimeFormat.TimeFormat = 'hh:mm:ss AM/PM'
        SQLDateTimeFormat.AlwaysY2k = False
        DisplayDateTimeFormat.DateSeparator = #0
        DisplayDateTimeFormat.TimeSeparator = #0
        DisplayDateTimeFormat.AlwaysY2k = True
        Items = <>
        OnChange = FilterBox1Change
        TabOrder = 0
        UpdateInThread = uiYesOnLoad
        UpdateOnLoaded = False
        ForceFilterDT = False
        WaitCursor = crSQLWait
        AdvancedFilter = True
        FilterOptions = []
        AddTableNames = False
        AllowedUsageIDs = '0'
        EncloseInBrackets = True
        Fields = <>
        FieldParams = <>
        Filtered = True
        OrderByItems = <>
      end
      object FilterCombo1: TComboBox
        Left = 0
        Top = 8
        Width = 524
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 1
        OnChange = FilterCombo1Change
      end
    end
    object Scenario2Sheet: TTabSheet
      Caption = 'Scenario2'
      ImageIndex = 1
      object FilterBox2: TPSCFltBox
        Left = 0
        Top = 32
        Width = 524
        Height = 129
        PopupParams.ClientEdge = True
        DefStoreMode = smText
        KeyMapping = 'Default'
        Options = [fboSortFields, fboSortConditions, fboCanAdd, fboCanDelete, fboCanIndent, fboCanOpen, fboCanSave, fboCanSaveToMem, fboCanLoadFromMem]
        ForceSQLDateTime = False
        ForceSQLBooleans = False
        FilterDateTimeFormat.DateSeparator = #0
        FilterDateTimeFormat.TimeSeparator = #0
        FilterDateTimeFormat.AlwaysY2k = True
        SQLDateTimeFormat.DateSeparator = '/'
        SQLDateTimeFormat.TimeSeparator = ':'
        SQLDateTimeFormat.AMSymbol = 'AM'
        SQLDateTimeFormat.PMSymbol = 'PM'
        SQLDateTimeFormat.DateFormat = 'MM/DD/YYYY'
        SQLDateTimeFormat.TimeFormat = 'hh:mm:ss AM/PM'
        SQLDateTimeFormat.AlwaysY2k = False
        DisplayDateTimeFormat.DateSeparator = #0
        DisplayDateTimeFormat.TimeSeparator = #0
        DisplayDateTimeFormat.AlwaysY2k = True
        Items = <>
        OnChange = FilterBox2Change
        TabOrder = 0
        UpdateInThread = uiYesOnLoad
        UpdateOnLoaded = False
        ForceFilterDT = False
        WaitCursor = crSQLWait
        AdvancedFilter = True
        FilterOptions = []
        AddTableNames = False
        AllowedUsageIDs = '0'
        EncloseInBrackets = True
        Fields = <>
        FieldParams = <>
        Filtered = True
        OrderByItems = <>
      end
      object FilterCombo2: TComboBox
        Left = 0
        Top = 8
        Width = 524
        Height = 21
        Style = csDropDownList
        ItemHeight = 0
        TabOrder = 1
        OnChange = FilterCombo2Change
      end
    end
    object Scenario3Sheet: TTabSheet
      Caption = 'Scenario3'
      ImageIndex = 2
      object FilterBox3: TPSCFltBox
        Left = 0
        Top = 32
        Width = 524
        Height = 129
        PopupParams.ClientEdge = True
        DefStoreMode = smText
        KeyMapping = 'Default'
        Options = [fboSortFields, fboSortConditions, fboCanAdd, fboCanDelete, fboCanIndent, fboCanOpen, fboCanSave, fboCanSaveToMem, fboCanLoadFromMem]
        ForceSQLDateTime = False
        ForceSQLBooleans = False
        FilterDateTimeFormat.DateSeparator = #0
        FilterDateTimeFormat.TimeSeparator = #0
        FilterDateTimeFormat.AlwaysY2k = True
        SQLDateTimeFormat.DateSeparator = '/'
        SQLDateTimeFormat.TimeSeparator = ':'
        SQLDateTimeFormat.AMSymbol = 'AM'
        SQLDateTimeFormat.PMSymbol = 'PM'
        SQLDateTimeFormat.DateFormat = 'MM/DD/YYYY'
        SQLDateTimeFormat.TimeFormat = 'hh:mm:ss AM/PM'
        SQLDateTimeFormat.AlwaysY2k = False
        DisplayDateTimeFormat.DateSeparator = #0
        DisplayDateTimeFormat.TimeSeparator = #0
        DisplayDateTimeFormat.AlwaysY2k = True
        Items = <>
        OnChange = FilterBox3Change
        TabOrder = 0
        UpdateInThread = uiYesOnLoad
        UpdateOnLoaded = False
        ForceFilterDT = False
        WaitCursor = crSQLWait
        AdvancedFilter = True
        FilterOptions = []
        AddTableNames = False
        AllowedUsageIDs = '0'
        EncloseInBrackets = True
        Fields = <>
        FieldParams = <>
        Filtered = True
        OrderByItems = <>
      end
      object FilterCombo3: TComboBox
        Left = 0
        Top = 8
        Width = 524
        Height = 21
        Style = csDropDownList
        ItemHeight = 0
        TabOrder = 1
        OnChange = FilterCombo3Change
      end
    end
    object Scenario4Sheet: TTabSheet
      Caption = 'Scenario4'
      ImageIndex = 3
      object FilterBox4: TPSCFltBox
        Left = 0
        Top = 32
        Width = 524
        Height = 129
        PopupParams.ClientEdge = True
        DefStoreMode = smText
        KeyMapping = 'Default'
        Options = [fboSortFields, fboSortConditions, fboCanAdd, fboCanDelete, fboCanIndent, fboCanOpen, fboCanSave, fboCanSaveToMem, fboCanLoadFromMem]
        ForceSQLDateTime = False
        ForceSQLBooleans = False
        FilterDateTimeFormat.DateSeparator = #0
        FilterDateTimeFormat.TimeSeparator = #0
        FilterDateTimeFormat.AlwaysY2k = True
        SQLDateTimeFormat.DateSeparator = '/'
        SQLDateTimeFormat.TimeSeparator = ':'
        SQLDateTimeFormat.AMSymbol = 'AM'
        SQLDateTimeFormat.PMSymbol = 'PM'
        SQLDateTimeFormat.DateFormat = 'MM/DD/YYYY'
        SQLDateTimeFormat.TimeFormat = 'hh:mm:ss AM/PM'
        SQLDateTimeFormat.AlwaysY2k = False
        DisplayDateTimeFormat.DateSeparator = #0
        DisplayDateTimeFormat.TimeSeparator = #0
        DisplayDateTimeFormat.AlwaysY2k = True
        Items = <>
        OnChange = FilterBox4Change
        TabOrder = 0
        UpdateInThread = uiYesOnLoad
        UpdateOnLoaded = False
        ForceFilterDT = False
        WaitCursor = crSQLWait
        AdvancedFilter = True
        FilterOptions = []
        AddTableNames = False
        AllowedUsageIDs = '0'
        EncloseInBrackets = True
        Fields = <>
        FieldParams = <>
        Filtered = True
        OrderByItems = <>
      end
      object FilterCombo4: TComboBox
        Left = 0
        Top = 8
        Width = 524
        Height = 21
        Style = csDropDownList
        ItemHeight = 0
        TabOrder = 1
        OnChange = FilterCombo4Change
      end
    end
    object Scenario5Sheet: TTabSheet
      Caption = 'Scenario5'
      ImageIndex = 4
      object FilterCombo5: TComboBox
        Left = 0
        Top = 8
        Width = 524
        Height = 21
        Style = csDropDownList
        ItemHeight = 0
        TabOrder = 0
        OnChange = FilterCombo5Change
      end
      object FilterBox5: TPSCFltBox
        Left = 0
        Top = 32
        Width = 524
        Height = 129
        PopupParams.ClientEdge = True
        DefStoreMode = smText
        KeyMapping = 'Default'
        Options = [fboSortFields, fboSortConditions, fboCanAdd, fboCanDelete, fboCanIndent, fboCanOpen, fboCanSave, fboCanSaveToMem, fboCanLoadFromMem]
        ForceSQLDateTime = False
        ForceSQLBooleans = False
        FilterDateTimeFormat.DateSeparator = #0
        FilterDateTimeFormat.TimeSeparator = #0
        FilterDateTimeFormat.AlwaysY2k = True
        SQLDateTimeFormat.DateSeparator = '/'
        SQLDateTimeFormat.TimeSeparator = ':'
        SQLDateTimeFormat.AMSymbol = 'AM'
        SQLDateTimeFormat.PMSymbol = 'PM'
        SQLDateTimeFormat.DateFormat = 'MM/DD/YYYY'
        SQLDateTimeFormat.TimeFormat = 'hh:mm:ss AM/PM'
        SQLDateTimeFormat.AlwaysY2k = False
        DisplayDateTimeFormat.DateSeparator = #0
        DisplayDateTimeFormat.TimeSeparator = #0
        DisplayDateTimeFormat.AlwaysY2k = True
        Items = <>
        OnChange = FilterBox5Change
        TabOrder = 1
        UpdateInThread = uiYesOnLoad
        UpdateOnLoaded = False
        ForceFilterDT = False
        WaitCursor = crSQLWait
        AdvancedFilter = True
        FilterOptions = []
        AddTableNames = False
        AllowedUsageIDs = '0'
        EncloseInBrackets = True
        Fields = <>
        FieldParams = <>
        Filtered = True
        OrderByItems = <>
      end
    end
  end
end
