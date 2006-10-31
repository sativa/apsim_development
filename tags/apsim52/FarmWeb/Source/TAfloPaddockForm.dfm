object AfloPaddockForm: TAfloPaddockForm
  Left = 0
  Top = 0
  Width = 1007
  Height = 659
  ConnectionMode = cmAny
  SupportedBrowsers = [brIE, brNetscape7, brOpera, brNetscape6]
  BrowserSecurityCheck = True
  Background.Fixed = False
  HandleTabs = False
  LockUntilLoaded = True
  LockOnSubmit = True
  ShowHint = True
  DesignLeft = 338
  DesignTop = 148
  object IWLabel2: TIWLabel
    Left = 170
    Top = 248
    Width = 50
    Height = 16
    Cursor = crAuto
    IW50Hint = False
    ParentShowHint = False
    ShowHint = True
    ZIndex = 0
    RenderSize = False
    Alignment = taLeftJustify
    BGColor = clNone
    Font.Color = clNone
    Font.FontName = 'Arial'
    Font.Size = 10
    Font.Style = []
    NoWrap = False
    FriendlyName = 'IWLabel1'
    Caption = 'Cultivar:'
    RawText = False
  end
  object IWLabel3: TIWLabel
    Left = 170
    Top = 216
    Width = 103
    Height = 16
    Cursor = crAuto
    IW50Hint = False
    ParentShowHint = False
    ShowHint = True
    ZIndex = 0
    RenderSize = False
    Alignment = taLeftJustify
    BGColor = clNone
    Font.Color = clNone
    Font.FontName = 'Arial'
    Font.Size = 10
    Font.Style = []
    NoWrap = False
    FriendlyName = 'IWLabel1'
    Caption = 'Date of planting:'
    RawText = False
  end
  object PlantingDate: TTIWDatePicker
    Left = 336
    Top = 216
    Width = 209
    Height = 21
    Cursor = crAuto
    IW50Hint = False
    ParentShowHint = False
    ShowHint = True
    ZIndex = 1
    RenderSize = False
    BackgroundImage.Stretch = False
    BackgroundImage.Frame = 0
    BorderCollapse = True
    BorderColor = clWebBLACK
    BorderWidth = 1
    CalendarFont.Color = clNone
    CalendarFont.Size = 10
    CalendarFont.Style = []
    CalendarHeight = 200
    CalendarWidth = 285
    Color = clWebWHITE
    DateFormat = dfEU
    DateSep = dsSlash
    Day = 16
    DropDownImage.Stretch = False
    DropDownImage.Frame = 0
    EditorEnabled = True
    Enabled = True
    Flat = False
    FocusColor = clWebWHITE
    Font.Color = clWebBLACK
    Font.Size = 10
    Font.Style = []
    GradientDirection = gdHorizontal
    GradientEndColor = clNone
    GradientStartColor = clNone
    GridLineColor = clWebSILVER
    GridLineWidth = 0
    HintNextMonth = 'Next Month'
    HintNextYear = 'Next Year'
    HintPrevMonth = 'Previous Month'
    HintPrevYear = 'Previous Year'
    HoverColor = clWebLIME
    Month = 9
    MonthColor = clInfoBk
    MonthGradientDirection = gdHorizontal
    MonthGradientEndColor = clNone
    MonthGradientStartColor = clNone
    MonthTextColor = clWebBLACK
    NameOfDays.Strings = (
      'Sun'
      'Mon'
      'Tue'
      'Wed'
      'Thu'
      'Fri'
      'Sat')
    NameOfMonths.Strings = (
      'January'
      'February'
      'March'
      'April'
      'May'
      'June'
      'July'
      'August'
      'September'
      'October'
      'November'
      'December')
    NullIsToday = False
    TabOrder = 0
    TodayColor = clWebRED
    StartDay = 1
    WeekendColor = clWebWHITE
    WeekendTextColor = clWebBLACK
    Year = 2003
  end
  object IWLabel4: TIWLabel
    Left = 170
    Top = 120
    Width = 55
    Height = 16
    Cursor = crAuto
    IW50Hint = False
    ParentShowHint = False
    ShowHint = True
    ZIndex = 0
    RenderSize = False
    Alignment = taRightJustify
    BGColor = clNone
    Font.Color = clNone
    Font.FontName = 'Arial'
    Font.Size = 10
    Font.Style = []
    NoWrap = False
    FriendlyName = 'IWLabel1'
    Caption = 'Soil type:'
    RawText = False
  end
  object IWLabel5: TIWLabel
    Left = 170
    Top = 280
    Width = 125
    Height = 16
    Cursor = crAuto
    IW50Hint = False
    ParentShowHint = False
    ShowHint = True
    ZIndex = 0
    RenderSize = False
    Alignment = taRightJustify
    BGColor = clNone
    Font.Color = clNone
    Font.FontName = 'Arial'
    Font.Size = 10
    Font.Style = []
    NoWrap = False
    FriendlyName = 'IWLabel1'
    Caption = 'Soil water at planting:'
    RawText = False
  end
  object SoilTempButton: TIWButton
    Left = 176
    Top = 328
    Width = 153
    Height = 25
    Cursor = crAuto
    IW50Hint = False
    ParentShowHint = False
    ShowHint = True
    ZIndex = 0
    RenderSize = True
    Caption = 'Enter soil temps'
    DoSubmitValidation = True
    Color = clBtnFace
    Font.Color = clNone
    Font.Size = 10
    Font.Style = []
    FriendlyName = 'IWButton1'
    ScriptEvents = <>
    TabOrder = 0
    OnClick = SoilTempButtonClick
  end
  object CultivarCombo: TIWComboBox
    Left = 336
    Top = 248
    Width = 209
    Height = 21
    Cursor = crAuto
    IW50Hint = False
    ParentShowHint = False
    ShowHint = True
    ZIndex = 0
    RenderSize = False
    BGColor = clNone
    Font.Color = clNone
    Font.Size = 10
    Font.Style = []
    FocusColor = clNone
    AutoHideOnMenuActivation = False
    ItemsHaveValues = False
    NoSelectionText = '-- No Selection --'
    Required = False
    RequireSelection = True
    ScriptEvents = <>
    UseSize = False
    DoSubmitValidation = True
    Editable = True
    NonEditableAsLabel = True
    TabOrder = 16
    ItemIndex = -1
    Sorted = False
    FriendlyName = 'CultivarCombo'
  end
  object IWLabel1: TIWLabel
    Left = 170
    Top = 88
    Width = 130
    Height = 16
    Cursor = crAuto
    IW50Hint = False
    ParentShowHint = False
    ShowHint = True
    ZIndex = 0
    RenderSize = False
    Alignment = taLeftJustify
    BGColor = clNone
    Font.Color = clNone
    Font.FontName = 'Arial'
    Font.Size = 10
    Font.Style = []
    NoWrap = False
    FriendlyName = 'IWLabel1'
    Caption = 'Closest weather stn:'
    RawText = False
  end
  object WeatherStationCombo: TIWComboBox
    Left = 336
    Top = 88
    Width = 209
    Height = 21
    Cursor = crAuto
    IW50Hint = False
    ParentShowHint = False
    ShowHint = True
    ZIndex = 0
    RenderSize = False
    BGColor = clNone
    Font.Color = clNone
    Font.Size = 10
    Font.Style = []
    FocusColor = clNone
    AutoHideOnMenuActivation = False
    ItemsHaveValues = False
    NoSelectionText = '-- No Selection --'
    Required = False
    RequireSelection = True
    ScriptEvents = <>
    UseSize = False
    DoSubmitValidation = True
    Editable = True
    NonEditableAsLabel = True
    TabOrder = 16
    ItemIndex = -1
    Sorted = False
    FriendlyName = 'VarietyCombo'
  end
  object SoilTypeCombo: TIWComboBox
    Left = 336
    Top = 120
    Width = 209
    Height = 21
    Cursor = crAuto
    IW50Hint = False
    ParentShowHint = False
    ShowHint = True
    ZIndex = 0
    RenderSize = False
    BGColor = clNone
    Font.Color = clNone
    Font.Size = 10
    Font.Style = []
    FocusColor = clNone
    AutoHideOnMenuActivation = False
    ItemsHaveValues = False
    NoSelectionText = '-- No Selection --'
    Required = False
    RequireSelection = True
    ScriptEvents = <>
    UseSize = False
    DoSubmitValidation = True
    Editable = True
    NonEditableAsLabel = True
    TabOrder = 16
    ItemIndex = -1
    Sorted = False
    FriendlyName = 'VarietyCombo'
  end
  object StartingSWCombo: TIWComboBox
    Left = 336
    Top = 280
    Width = 209
    Height = 21
    Cursor = crAuto
    IW50Hint = False
    ParentShowHint = False
    ShowHint = True
    ZIndex = 0
    RenderSize = False
    BGColor = clNone
    Font.Color = clNone
    Font.Size = 10
    Font.Style = []
    FocusColor = clNone
    AutoHideOnMenuActivation = False
    ItemsHaveValues = False
    NoSelectionText = '-- No Selection --'
    Required = False
    RequireSelection = True
    ScriptEvents = <>
    UseSize = False
    DoSubmitValidation = True
    Editable = True
    NonEditableAsLabel = True
    TabOrder = 16
    ItemIndex = -1
    Sorted = False
    FriendlyName = 'VarietyCombo'
  end
  object AirTempButton: TIWButton
    Left = 344
    Top = 328
    Width = 153
    Height = 25
    Cursor = crAuto
    IW50Hint = False
    ParentShowHint = False
    ShowHint = True
    ZIndex = 0
    RenderSize = True
    Caption = 'Enter air temps'
    DoSubmitValidation = True
    Color = clBtnFace
    Font.Color = clNone
    Font.Size = 10
    Font.Style = []
    FriendlyName = 'IWButton1'
    ScriptEvents = <>
    TabOrder = 0
    OnClick = AirTempButtonClick
  end
  object IWRectangle1: TIWRectangle
    Left = 0
    Top = 0
    Width = 1007
    Height = 38
    Cursor = crAuto
    Align = alTop
    IW50Hint = False
    ParentShowHint = False
    ShowHint = True
    ZIndex = 0
    RenderSize = True
    Font.Color = clNone
    Font.Size = 10
    Font.Style = []
    BorderOptions.Color = clNone
    BorderOptions.Width = 0
    FriendlyName = 'IWRectangle1'
    Color = clWebBEIGE
    Alignment = taLeftJustify
    VAlign = vaMiddle
  end
  object UserLabel: TIWLabel
    Left = 170
    Top = 56
    Width = 368
    Height = 16
    Cursor = crAuto
    IW50Hint = False
    ParentShowHint = False
    ShowHint = True
    ZIndex = 0
    RenderSize = False
    Alignment = taLeftJustify
    BGColor = clNone
    Font.Color = clNone
    Font.FontName = 'Arial'
    Font.Size = 10
    Font.Style = [fsBold]
    NoWrap = False
    FriendlyName = 'IWLabel1'
    Caption = 'In crop management for user: xxx and paddock: yyy.'
    RawText = False
  end
  object IWImageFile1: TIWImageFile
    Left = 168
    Top = 8
    Width = 24
    Height = 24
    Cursor = crAuto
    IW50Hint = False
    ParentShowHint = False
    ShowHint = True
    ZIndex = 0
    RenderSize = False
    BorderOptions.Color = clNone
    BorderOptions.Width = 0
    DoSubmitValidation = True
    ScriptEvents = <>
    TabOrder = 22
    UseSize = False
    FriendlyName = 'IWImageFile1'
    ImageFile.Filename = 'd:\development\FarmWeb\Source\Files\disk_blue.gif'
  end
  object SaveButton: TIWLink
    Left = 195
    Top = 11
    Width = 46
    Height = 17
    Cursor = crAuto
    IW50Hint = False
    ParentShowHint = False
    ShowHint = True
    ZIndex = 0
    RenderSize = False
    Caption = 'Save'
    Color = clNone
    Font.Color = clWebBLUE
    Font.FontName = 'Arial'
    Font.Size = 10
    Font.Style = [fsUnderline]
    ScriptEvents = <>
    DoSubmitValidation = False
    FriendlyName = 'SaveButton'
    OnClick = SaveButtonClick
    TabOrder = 23
    RawText = False
  end
  object RainfallButton: TIWLink
    Left = 275
    Top = 11
    Width = 70
    Height = 17
    Cursor = crAuto
    IW50Hint = False
    ParentShowHint = False
    ShowHint = True
    ZIndex = 0
    RenderSize = False
    Caption = 'Rainfall'
    Color = clNone
    Font.Color = clWebBLUE
    Font.FontName = 'Arial'
    Font.Size = 10
    Font.Style = [fsUnderline]
    ScriptEvents = <>
    DoSubmitValidation = False
    FriendlyName = 'SaveButton'
    OnClick = RainfallEntryButtonClick
    TabOrder = 23
    RawText = False
  end
  object IWImageFile4: TIWImageFile
    Left = 248
    Top = 8
    Width = 21
    Height = 22
    Cursor = crAuto
    IW50Hint = False
    ParentShowHint = False
    ShowHint = True
    ZIndex = 0
    RenderSize = False
    BorderOptions.Color = clNone
    BorderOptions.Width = 0
    DoSubmitValidation = True
    ScriptEvents = <>
    TabOrder = 22
    UseSize = False
    FriendlyName = 'IWImageFile1'
    ImageFile.Filename = 'd:\development\FarmWeb\Source\Files\rainfall.gif'
  end
  object IWImageFile5: TIWImageFile
    Left = 432
    Top = 8
    Width = 24
    Height = 24
    Cursor = crAuto
    IW50Hint = False
    ParentShowHint = False
    ShowHint = True
    ZIndex = 0
    RenderSize = False
    BorderOptions.Color = clNone
    BorderOptions.Width = 0
    DoSubmitValidation = True
    ScriptEvents = <>
    TabOrder = 22
    UseSize = False
    FriendlyName = 'IWImageFile1'
    ImageFile.Filename = 'd:\development\FarmWeb\Source\Files\column-chart.gif'
  end
  object CreateReportButton: TIWLink
    Left = 459
    Top = 11
    Width = 110
    Height = 17
    Cursor = crAuto
    IW50Hint = False
    ParentShowHint = False
    ShowHint = True
    ZIndex = 0
    RenderSize = False
    Caption = 'Create report'
    Color = clNone
    Font.Color = clWebBLUE
    Font.FontName = 'Arial'
    Font.Size = 10
    Font.Style = [fsUnderline]
    ScriptEvents = <>
    DoSubmitValidation = False
    FriendlyName = 'SaveButton'
    OnClick = CreateReportButtonClick
    TabOrder = 23
    RawText = False
  end
  object IWLabel7: TIWLabel
    Left = 566
    Top = 88
    Width = 255
    Height = 16
    Cursor = crAuto
    IW50Hint = False
    ParentShowHint = False
    ShowHint = True
    ZIndex = 0
    RenderSize = False
    Alignment = taLeftJustify
    BGColor = clNone
    Font.Color = clWebBLUE
    Font.FontName = 'Arial'
    Font.Size = 10
    Font.Style = []
    NoWrap = False
    FriendlyName = 'IWLabel1'
    Caption = 'To request a new report for this paddock'
    RawText = False
  end
  object IWLabel8: TIWLabel
    Left = 566
    Top = 112
    Width = 260
    Height = 16
    Cursor = crAuto
    IW50Hint = False
    ParentShowHint = False
    ShowHint = True
    ZIndex = 0
    RenderSize = False
    Alignment = taRightJustify
    BGColor = clNone
    Font.Color = clWebBLUE
    Font.FontName = 'Arial'
    Font.Size = 10
    Font.Style = []
    NoWrap = False
    FriendlyName = 'IWLabel1'
    Caption = 'click on '#39'create report'#39' at the top of  this page'
    RawText = False
  end
  object HelpImage: TIWImageFile
    Left = 8
    Top = 8
    Width = 24
    Height = 24
    Cursor = crAuto
    IW50Hint = False
    ParentShowHint = False
    ShowHint = True
    ZIndex = 0
    RenderSize = True
    AutoSize = False
    BorderOptions.Color = clNone
    BorderOptions.Width = 0
    DoSubmitValidation = True
    ScriptEvents = <>
    TabOrder = 22
    UseSize = True
    FriendlyName = 'IWImageFile1'
    ImageFile.Filename = 'd:\development\FarmWeb\Source\Files\help2.gif'
  end
  object HelpButton: TIWLink
    Left = 35
    Top = 11
    Width = 46
    Height = 17
    Cursor = crAuto
    IW50Hint = False
    ParentShowHint = False
    ShowHint = True
    ZIndex = 0
    RenderSize = False
    Caption = 'Help'
    Color = clNone
    Font.Color = clWebBLUE
    Font.FontName = 'Arial'
    Font.Size = 10
    Font.Style = [fsUnderline]
    ScriptEvents = <>
    DoSubmitValidation = False
    FriendlyName = 'SaveButton'
    OnClick = HelpButtonClick
    TabOrder = 23
    RawText = False
  end
  object IrrigationButton: TIWLink
    Left = 363
    Top = 11
    Width = 70
    Height = 17
    Cursor = crAuto
    IW50Hint = False
    ParentShowHint = False
    ShowHint = True
    ZIndex = 0
    RenderSize = False
    Caption = 'Irrigation'
    Color = clNone
    Font.Color = clWebBLUE
    Font.FontName = 'Arial'
    Font.Size = 10
    Font.Style = [fsUnderline]
    ScriptEvents = <>
    DoSubmitValidation = False
    FriendlyName = 'SaveButton'
    OnClick = IrrigationButtonClick
    TabOrder = 23
    RawText = False
  end
  object IWImageFile2: TIWImageFile
    Left = 336
    Top = 8
    Width = 21
    Height = 22
    Cursor = crAuto
    IW50Hint = False
    ParentShowHint = False
    ShowHint = True
    ZIndex = 0
    RenderSize = False
    BorderOptions.Color = clNone
    BorderOptions.Width = 0
    DoSubmitValidation = True
    ScriptEvents = <>
    TabOrder = 22
    UseSize = False
    FriendlyName = 'IWImageFile1'
    ImageFile.Filename = 'D:\development\FarmWeb\Source\Files\irrigation.GIF'
  end
  object IRButton1: TIWButton
    Left = 568
    Top = 152
    Width = 177
    Height = 33
    Cursor = crAuto
    IW50Hint = False
    ParentShowHint = False
    ShowHint = True
    ZIndex = 0
    RenderSize = True
    Caption = 'View RS image 1'
    DoSubmitValidation = True
    Color = clBtnFace
    Font.Color = clNone
    Font.Size = 10
    Font.Style = []
    FriendlyName = 'IRButton1'
    ScriptEvents = <>
    TabOrder = 0
    OnClick = IRButton1Click
  end
  object IRButton2: TIWButton
    Left = 568
    Top = 192
    Width = 177
    Height = 33
    Cursor = crAuto
    IW50Hint = False
    ParentShowHint = False
    ShowHint = True
    ZIndex = 0
    RenderSize = True
    Caption = 'View RS image 2'
    DoSubmitValidation = True
    Color = clBtnFace
    Font.Color = clNone
    Font.Size = 10
    Font.Style = []
    FriendlyName = 'IRButton1'
    ScriptEvents = <>
    TabOrder = 0
    OnClick = IRButton2Click
  end
end
