object AfloPaddockForm: TAfloPaddockForm
  Left = 0
  Top = 0
  Width = 857
  Height = 519
  ConnectionMode = cmAny
  SupportedBrowsers = [brIE, brNetscape7, brOpera, brNetscape6]
  BrowserSecurityCheck = True
  Background.Fixed = False
  HandleTabs = False
  LockUntilLoaded = True
  LockOnSubmit = True
  ShowHint = True
  UpdateMode = umAll
  DesignSize = (
    857
    519)
  DesignLeft = 217
  DesignTop = 226
  object IWLabel2: TIWLabel
    Left = 146
    Top = 216
    Width = 64
    Height = 19
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
    Left = 146
    Top = 184
    Width = 130
    Height = 19
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
    Left = 312
    Top = 184
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
    Left = 146
    Top = 88
    Width = 68
    Height = 19
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
    Left = 146
    Top = 248
    Width = 155
    Height = 19
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
  object RainfallEntryButton: TIWButton
    Left = 232
    Top = 304
    Width = 153
    Height = 25
    Cursor = crAuto
    IW50Hint = False
    ParentShowHint = False
    ShowHint = True
    ZIndex = 0
    RenderSize = True
    Caption = 'Enter Rainfall'
    DoSubmitValidation = True
    Color = clBtnFace
    Font.Color = clNone
    Font.Size = 10
    Font.Style = []
    FriendlyName = 'IWButton1'
    ScriptEvents = <>
    TabOrder = 0
    OnClick = RainfallEntryButtonClick
  end
  object UserLabel: TIWLabel
    Left = 144
    Top = 8
    Width = 372
    Height = 19
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
    Caption = 'Paddock details for user xxx and paddock yyy.'
    RawText = False
  end
  object SaveButton: TIWButton
    Left = 536
    Top = 248
    Width = 129
    Height = 25
    Cursor = crAuto
    IW50Hint = False
    ParentShowHint = False
    ShowHint = True
    ZIndex = 0
    RenderSize = True
    Caption = 'Save changes'
    DoSubmitValidation = True
    Color = clBtnFace
    Font.Color = clNone
    Font.Size = 10
    Font.Style = []
    FriendlyName = 'IWButton1'
    ScriptEvents = <>
    TabOrder = 0
    OnClick = SaveButtonClick
  end
  object SoilTempButton: TIWButton
    Left = 400
    Top = 304
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
    Left = 312
    Top = 216
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
    Left = 146
    Top = 56
    Width = 160
    Height = 19
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
    Left = 312
    Top = 56
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
    Left = 312
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
  object SoilDepthCombo: TIWComboBox
    Left = 312
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
  object IWLabel6: TIWLabel
    Left = 146
    Top = 120
    Width = 77
    Height = 19
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
    Caption = 'Soil depth:'
    RawText = False
  end
  object StartingSWCombo: TIWComboBox
    Left = 312
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
    FriendlyName = 'VarietyCombo'
  end
  object AirTempButton: TIWButton
    Left = 568
    Top = 304
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
  object Bar: TTIWOutlookBar
    Left = 0
    Top = 0
    Width = 129
    Height = 490
    Cursor = crAuto
    Anchors = [akLeft, akTop, akBottom]
    IW50Hint = False
    ParentShowHint = False
    ShowHint = True
    ZIndex = 0
    RenderSize = True
    ActivePanel = 0
    BackgroundGradientDirection = gdVertical
    BorderSize = 1
    ButtonColor = clBtnFace
    ButtonColorTo = clNone
    ButtonFont.Color = clWebBLACK
    ButtonFont.FontName = 'Arial'
    ButtonFont.FontFamily = 'Arial, Sans-Serif, Verdana'
    ButtonFont.Size = 12
    ButtonFont.Style = []
    ButtonGradientDirection = gdHorizontal
    ButtonHeight = 20
    Color = clWebBEIGE
    ColorTo = clWebLIGHTGRAY
    HoverColor = clWebRED
    ImageBorderColor = clInfoBk
    ImageHeight = 32
    ImageWidth = 32
    ImageMargin = 2
    ItemFont.Color = clWebBLACK
    ItemFont.FontName = 'Arial'
    ItemFont.FontFamily = 'Arial, Sans-Serif, Verdana'
    ItemFont.Size = 12
    ItemFont.Style = []
    ItemHeight = 90
    ItemWidth = 40
    LabelMargin = 0
    OpenCloseSpeed = 3
    Panels = <>
    UseTemplate = False
  end
  object EmailFilesButton: TIWButton
    Left = 144
    Top = 464
    Width = 161
    Height = 25
    Cursor = crAuto
    IW50Hint = False
    ParentShowHint = False
    ShowHint = True
    ZIndex = 0
    RenderSize = True
    Caption = 'Email report files to:'
    DoSubmitValidation = True
    Color = clBtnFace
    Font.Color = clNone
    Font.Size = 10
    Font.Style = []
    FriendlyName = 'RequestButton'
    ScriptEvents = <>
    TabOrder = 0
    OnClick = EmailFilesButtonClick
  end
  object EmailFilesEdit: TIWEdit
    Left = 144
    Top = 440
    Width = 265
    Height = 21
    Cursor = crAuto
    IW50Hint = False
    ParentShowHint = False
    ShowHint = True
    ZIndex = 0
    RenderSize = True
    Alignment = taLeftJustify
    BGColor = clNone
    FocusColor = clNone
    DoSubmitValidation = True
    Editable = True
    NonEditableAsLabel = True
    Font.Color = clNone
    Font.Size = 10
    Font.Style = []
    FriendlyName = 'EmailFilesEdit'
    MaxLength = 0
    ReadOnly = False
    Required = False
    ScriptEvents = <>
    TabOrder = 20
    PasswordPrompt = False
  end
  object ReportCombo: TIWComboBox
    Left = 144
    Top = 388
    Width = 201
    Height = 21
    Cursor = crAuto
    IW50Hint = False
    ParentShowHint = False
    ShowHint = True
    ZIndex = 0
    RenderSize = True
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
    UseSize = True
    DoSubmitValidation = True
    Editable = True
    NonEditableAsLabel = True
    TabOrder = 21
    ItemIndex = -1
    Sorted = False
    FriendlyName = 'ReportCombo'
  end
  object RequestButton: TIWButton
    Left = 144
    Top = 410
    Width = 185
    Height = 25
    Cursor = crAuto
    IW50Hint = False
    ParentShowHint = False
    ShowHint = True
    ZIndex = 0
    RenderSize = True
    Caption = 'Generate report'
    DoSubmitValidation = True
    Color = clBtnFace
    Font.Color = clNone
    Font.Size = 10
    Font.Style = []
    FriendlyName = 'RequestButton'
    ScriptEvents = <>
    TabOrder = 0
    OnClick = RequestButtonClick
  end
  object IWLabel7: TIWLabel
    Left = 146
    Top = 344
    Width = 546
    Height = 19
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
    Caption = 
      'To request a new report be generated for this paddock, select a ' +
      'report type'
    RawText = False
  end
  object IWLabel8: TIWLabel
    Left = 146
    Top = 360
    Width = 247
    Height = 19
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
    Caption = 'and hit the generate report button.'
    RawText = False
  end
end
