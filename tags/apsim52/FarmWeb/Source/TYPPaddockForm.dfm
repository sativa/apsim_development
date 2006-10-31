object YPPaddockForm: TYPPaddockForm
  Left = 0
  Top = 0
  Width = 999
  Height = 720
  ConnectionMode = cmAny
  Title = 'Yield Prophet'
  SupportedBrowsers = [brIE, brNetscape7, brOpera, brNetscape6]
  BrowserSecurityCheck = True
  Background.Fixed = False
  HandleTabs = False
  LockUntilLoaded = True
  LockOnSubmit = True
  ShowHint = True
  DesignLeft = 376
  DesignTop = 159
  object IWRectangle1: TIWRectangle
    Left = 0
    Top = 0
    Width = 999
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
  object CultivarLabel: TIWLabel
    Left = 298
    Top = 128
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
    Top = 96
    Width = 125
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
    Caption = 'Have sown yet?'
    RawText = False
  end
  object PlantingDate: TTIWDatePicker
    Left = 368
    Top = 96
    Width = 177
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
    Day = 1
    DropDownImage.Stretch = False
    DropDownImage.Frame = 0
    EditorEnabled = True
    Enabled = True
    Flat = False
    FocusColor = clWebWHITE
    Font.Color = clWebBLACK
    Font.FontName = 'Arial'
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
    Month = 4
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
    NullIsToday = True
    TabOrder = 0
    TodayColor = clWebRED
    StartDay = 1
    WeekendColor = clWebWHITE
    WeekendTextColor = clWebBLACK
    Year = 2004
  end
  object UserLabel: TIWLabel
    Left = 146
    Top = 56
    Width = 455
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
    Font.Style = [fsBold]
    NoWrap = False
    FriendlyName = 'IWLabel1'
    Caption = 'In crop management for user: xxx and paddock: yyy.'
    RawText = False
  end
  object CultivarCombo: TIWComboBox
    Left = 368
    Top = 128
    Width = 177
    Height = 21
    Cursor = crAuto
    IW50Hint = False
    ParentShowHint = False
    ShowHint = True
    ZIndex = 0
    RenderSize = False
    BGColor = clNone
    Font.Color = clNone
    Font.FontName = 'Arial'
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
    TabOrder = 13
    ItemIndex = -1
    Sorted = False
    FriendlyName = 'CultivarCombo'
  end
  object ReportCombo: TIWComboBox
    Left = 510
    Top = 11
    Width = 219
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
    TabOrder = 18
    ItemIndex = -1
    Sorted = False
    FriendlyName = 'ReportCombo'
  end
  object IWLabel7: TIWLabel
    Left = 584
    Top = 224
    Width = 324
    Height = 19
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
  object IWLabel6: TIWLabel
    Left = 146
    Top = 184
    Width = 294
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
    Caption = 'Actual nitrogen fertiliser applications:'
    RawText = False
  end
  object PlantingDateCheck: TIWCheckBox
    Left = 272
    Top = 96
    Width = 25
    Height = 21
    Cursor = crAuto
    IW50Hint = False
    ParentShowHint = False
    ShowHint = True
    ZIndex = 0
    RenderSize = False
    Editable = True
    Font.Color = clNone
    Font.Size = 10
    Font.Style = []
    ScriptEvents = <>
    DoSubmitValidation = True
    Style = stNormal
    TabOrder = 21
    OnClick = PlantingDateCheckClick
    Checked = False
    FriendlyName = 'FertCheck1'
  end
  object DateLabel: TIWLabel
    Left = 298
    Top = 96
    Width = 42
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
    Caption = 'Date:'
    RawText = False
  end
  object IWLabel2: TIWLabel
    Left = 606
    Top = 248
    Width = 265
    Height = 19
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
    Caption = ' select a report type and click on the'
    RawText = False
  end
  object FertGrid: TTIWAdvWebGrid
    Left = 144
    Top = 208
    Width = 321
    Height = 161
    Cursor = crAuto
    IW50Hint = False
    ParentShowHint = False
    ShowHint = True
    ZIndex = 0
    RenderSize = False
    ActiveRowColor = clWebWHITE
    ActiveRowFontColor = clNone
    AdvanceOnReturn = True
    AutoEdit = True
    AutoHTMLAdapt = False
    Background.GradientDirection = gdHorizontal
    Background.Gradient1 = clNone
    Background.Gradient2 = clNone
    Background.Picture.Stretch = False
    Background.Picture.Frame = 0
    Bands.Active = True
    Bands.PrimaryColor = clInfoBk
    Bands.SecondaryColor = clWebWHITE
    Borders.Inner = ibAll
    Borders.Outer = obAll
    Borders.Padding = 0
    Borders.Spacing = 0
    Borders.Width = 1
    Borders.Collapsed = True
    Borders.Color = clWebLIGHTSKYBLUE
    Borders.ColorDark = clNone
    Borders.ColorLight = clNone
    CheckTruePicture.Stretch = False
    CheckTruePicture.Frame = 0
    CheckFalsePicture.Stretch = False
    CheckFalsePicture.Frame = 0
    Color = clWebBLACK
    Columns = <
      item
        Alignment = taLeftJustify
        AllowSizing = False
        ButtonWidth = 0
        CheckTrue = 'true'
        CheckFalse = 'false'
        Color = clNone
        ColumnHeaderAlignment = taLeftJustify
        ColumnHeaderClick = False
        ColumnHeaderColor = clNone
        ColumnHeaderFont.Color = clNone
        ColumnHeaderFont.Size = 10
        ColumnHeaderFont.Style = []
        ColumnHeaderGradient1 = clNone
        ColumnHeaderGradient2 = clNone
        ColumnHeaderGradientDirection = gdHorizontal
        ColumnHeaderCheckBox = False
        ColumnHeaderNode = False
        ColumnType = ctNormal
        DataButtonType = dbtButton
        DetailSpan = 0
        DynPrecision = 0
        DynEditor = deText
        ImageIndex = -1
        Editor = edDatePicker
        Filter = False
        FilterIndex = 0
        Font.Color = clNone
        Font.Size = 10
        Font.Style = []
        FooterAlignment = taLeftJustify
        FooterFormat = '%g'
        FooterGradient1 = clNone
        FooterGradient2 = clNone
        FooterType = ftNone
        MaxLength = 0
        PopupColor = clWebWHITE
        PopupColorTo = clNone
        PopupColorGradientDirection = gdHorizontal
        ImageHeight = 0
        ImageWidth = 0
        PopupHeight = 200
        PopupWidth = 200
        ProgressColor = clWebRED
        ShowHint = False
        SpinEditMaxValue = 100
        SpinEditMinValue = 0
        SubTitleSpan = 0
        SubTitleVAlign = vaNone
        Tag = 0
        Title = 'Application date'
        TitleRowSpan = False
        TitleSpan = 0
        TitleVAlign = vaNone
        VAlign = vaNone
        Visible = True
        Width = 150
        WidthType = wtAbsolute
        SortFormat = sfAlphabetic
      end
      item
        Alignment = taLeftJustify
        AllowSizing = False
        ButtonWidth = 0
        CheckTrue = 'true'
        CheckFalse = 'false'
        Color = clNone
        ColumnHeaderAlignment = taRightJustify
        ColumnHeaderClick = False
        ColumnHeaderColor = clNone
        ColumnHeaderFont.Color = clNone
        ColumnHeaderFont.Size = 10
        ColumnHeaderFont.Style = []
        ColumnHeaderGradient1 = clNone
        ColumnHeaderGradient2 = clNone
        ColumnHeaderGradientDirection = gdHorizontal
        ColumnHeaderCheckBox = False
        ColumnHeaderNode = False
        ColumnType = ctNormal
        DataButtonType = dbtButton
        DetailSpan = 0
        DynPrecision = 0
        DynEditor = deText
        ImageIndex = -1
        Editor = edEditFloat
        Filter = False
        FilterIndex = 0
        Font.Color = clNone
        Font.Size = 10
        Font.Style = []
        FooterAlignment = taLeftJustify
        FooterFormat = '%g'
        FooterGradient1 = clNone
        FooterGradient2 = clNone
        FooterType = ftNone
        MaxLength = 0
        PopupColor = clWebWHITE
        PopupColorTo = clNone
        PopupColorGradientDirection = gdHorizontal
        ImageHeight = 0
        ImageWidth = 0
        PopupHeight = 200
        PopupWidth = 200
        ProgressColor = clWebRED
        ShowHint = False
        SpinEditMaxValue = 100
        SpinEditMinValue = 0
        SubTitle = '(kg/ha)'
        SubTitleSpan = 0
        SubTitleVAlign = vaNone
        Tag = 0
        Title = 'Amount N applied'
        TitleRowSpan = False
        TitleSpan = 0
        TitleVAlign = vaNone
        VAlign = vaNone
        Visible = True
        Width = 150
        WidthType = wtAbsolute
        SortFormat = sfAlphabetic
      end>
    ColumnHeaderColor = clWebLIGHTBLUE
    ColumnHeaderFont.Color = clNone
    ColumnHeaderFont.Size = 10
    ColumnHeaderFont.Style = []
    ColumnHeaderBorders.Inner = ibAll
    ColumnHeaderBorders.Outer = obAll
    ColumnHeaderBorders.Padding = 0
    ColumnHeaderBorders.Spacing = 0
    ColumnHeaderBorders.Width = 1
    ColumnHeaderBorders.Collapsed = True
    ColumnHeaderBorders.Color = clWebLIGHTSKYBLUE
    ColumnHeaderBorders.ColorDark = clNone
    ColumnHeaderBorders.ColorLight = clNone
    ColumnSizing = True
    Controller.Alignment = taLeftJustify
    Controller.Borders.Inner = ibAll
    Controller.Borders.Outer = obAll
    Controller.Borders.Padding = 0
    Controller.Borders.Spacing = 0
    Controller.Borders.Width = 1
    Controller.Borders.Collapsed = True
    Controller.Borders.Color = clWebLIGHTSKYBLUE
    Controller.Borders.ColorDark = clNone
    Controller.Borders.ColorLight = clNone
    Controller.Color = clWebLIGHTSKYBLUE
    Controller.Font.Color = clNone
    Controller.Font.Size = 10
    Controller.Font.Style = []
    Controller.Gradient1 = clNone
    Controller.Gradient2 = clNone
    Controller.GradientDirection = gdHorizontal
    Controller.Height = 22
    Controller.MaxPages = 10
    Controller.Position = cpNone
    Controller.Pager = cpPrevNext
    Controller.PagerType = cptLink
    Controller.IndicatorFormat = '%d of %d'
    Controller.IndicatorType = itNone
    Controller.TextPrev = 'Prev'
    Controller.TextNext = 'Next'
    Controller.TextFirst = 'First'
    Controller.TextLast = 'Last'
    Controller.ImgPrev.Stretch = False
    Controller.ImgPrev.Frame = 0
    Controller.ImgNext.Stretch = False
    Controller.ImgNext.Frame = 0
    Controller.ImgFirst.Stretch = False
    Controller.ImgFirst.Frame = 0
    Controller.ImgLast.Stretch = False
    Controller.ImgLast.Frame = 0
    Controller.RowCountSelect = False
    Controller.HintFind = 'Find'
    Controller.ShowPagersAlways = False
    DateSeparator = '/'
    DateFormat = gdEU
    DecimalPoint = '.'
    DefaultRowHeight = 18
    DetailRowHeight = 0
    DetailRowShow = dsNormal
    EditColor = clNone
    EditSelectAll = False
    FooterBorders.Inner = ibAll
    FooterBorders.Outer = obAll
    FooterBorders.Padding = 0
    FooterBorders.Spacing = 0
    FooterBorders.Width = 1
    FooterBorders.Collapsed = True
    FooterBorders.Color = clNone
    FooterBorders.ColorDark = clNone
    FooterBorders.ColorLight = clNone
    FooterColor = clBtnFace
    FooterFont.Color = clNone
    FooterFont.Size = 10
    FooterFont.Style = []
    Font.Color = clNone
    Font.FontName = 'Arial'
    Font.FontFamily = 'Arial, Sans-Serif, Verdana'
    Font.Size = 10
    Font.Style = []
    Glyphs.EditButton.Data = {
      36050000424D3605000000000000360400002800000010000000100000000100
      0800000000000001000000000000000000000001000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000C0DCC000F0CA
      A6000020400000206000002080000020A0000020C0000020E000004000000040
      20000040400000406000004080000040A0000040C0000040E000006000000060
      20000060400000606000006080000060A0000060C0000060E000008000000080
      20000080400000806000008080000080A0000080C0000080E00000A0000000A0
      200000A0400000A0600000A0800000A0A00000A0C00000A0E00000C0000000C0
      200000C0400000C0600000C0800000C0A00000C0C00000C0E00000E0000000E0
      200000E0400000E0600000E0800000E0A00000E0C00000E0E000400000004000
      20004000400040006000400080004000A0004000C0004000E000402000004020
      20004020400040206000402080004020A0004020C0004020E000404000004040
      20004040400040406000404080004040A0004040C0004040E000406000004060
      20004060400040606000406080004060A0004060C0004060E000408000004080
      20004080400040806000408080004080A0004080C0004080E00040A0000040A0
      200040A0400040A0600040A0800040A0A00040A0C00040A0E00040C0000040C0
      200040C0400040C0600040C0800040C0A00040C0C00040C0E00040E0000040E0
      200040E0400040E0600040E0800040E0A00040E0C00040E0E000800000008000
      20008000400080006000800080008000A0008000C0008000E000802000008020
      20008020400080206000802080008020A0008020C0008020E000804000008040
      20008040400080406000804080008040A0008040C0008040E000806000008060
      20008060400080606000806080008060A0008060C0008060E000808000008080
      20008080400080806000808080008080A0008080C0008080E00080A0000080A0
      200080A0400080A0600080A0800080A0A00080A0C00080A0E00080C0000080C0
      200080C0400080C0600080C0800080C0A00080C0C00080C0E00080E0000080E0
      200080E0400080E0600080E0800080E0A00080E0C00080E0E000C0000000C000
      2000C0004000C0006000C0008000C000A000C000C000C000E000C0200000C020
      2000C0204000C0206000C0208000C020A000C020C000C020E000C0400000C040
      2000C0404000C0406000C0408000C040A000C040C000C040E000C0600000C060
      2000C0604000C0606000C0608000C060A000C060C000C060E000C0800000C080
      2000C0804000C0806000C0808000C080A000C080C000C080E000C0A00000C0A0
      2000C0A04000C0A06000C0A08000C0A0A000C0A0C000C0A0E000C0C00000C0C0
      2000C0C04000C0C06000C0C08000C0C0A000F0FBFF00A4A0A000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000707E4E4E4E4
      E4E4E4E4E4E4E4E4E4070707EDF609090909090909090909E4070707EDF60707
      0707070707070709E4070707EDF6F6090909090909090909E4070707EDFFF609
      0909095A09090909E4070707F5FF07070707075200070709E4070707F5FFFFF6
      F609095A00000709E407070707FFFFFFF6F6095AFB360007E407070707FF0707
      070707A4FB360007ED07070709FFFFFFFFF6F6F65BFB3600ED07070709FFFFFF
      FFFFF6F6ACFB36000707070709FF070707070707F652FB360007070709FFFFFF
      FFFFFFFF079A7F360007070709FFFFFFFFFFFFFF08EC51000200070709FFFFFF
      FFFFFFFF07EC9A0202410707090909090909090907E407838307}
    Glyphs.EditHint = 'Edit'
    Glyphs.PostButton.Data = {
      36050000424D3605000000000000360400002800000010000000100000000100
      0800000000000001000000000000000000000001000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000C0DCC000F0CA
      A6000020400000206000002080000020A0000020C0000020E000004000000040
      20000040400000406000004080000040A0000040C0000040E000006000000060
      20000060400000606000006080000060A0000060C0000060E000008000000080
      20000080400000806000008080000080A0000080C0000080E00000A0000000A0
      200000A0400000A0600000A0800000A0A00000A0C00000A0E00000C0000000C0
      200000C0400000C0600000C0800000C0A00000C0C00000C0E00000E0000000E0
      200000E0400000E0600000E0800000E0A00000E0C00000E0E000400000004000
      20004000400040006000400080004000A0004000C0004000E000402000004020
      20004020400040206000402080004020A0004020C0004020E000404000004040
      20004040400040406000404080004040A0004040C0004040E000406000004060
      20004060400040606000406080004060A0004060C0004060E000408000004080
      20004080400040806000408080004080A0004080C0004080E00040A0000040A0
      200040A0400040A0600040A0800040A0A00040A0C00040A0E00040C0000040C0
      200040C0400040C0600040C0800040C0A00040C0C00040C0E00040E0000040E0
      200040E0400040E0600040E0800040E0A00040E0C00040E0E000800000008000
      20008000400080006000800080008000A0008000C0008000E000802000008020
      20008020400080206000802080008020A0008020C0008020E000804000008040
      20008040400080406000804080008040A0008040C0008040E000806000008060
      20008060400080606000806080008060A0008060C0008060E000808000008080
      20008080400080806000808080008080A0008080C0008080E00080A0000080A0
      200080A0400080A0600080A0800080A0A00080A0C00080A0E00080C0000080C0
      200080C0400080C0600080C0800080C0A00080C0C00080C0E00080E0000080E0
      200080E0400080E0600080E0800080E0A00080E0C00080E0E000C0000000C000
      2000C0004000C0006000C0008000C000A000C000C000C000E000C0200000C020
      2000C0204000C0206000C0208000C020A000C020C000C020E000C0400000C040
      2000C0404000C0406000C0408000C040A000C040C000C040E000C0600000C060
      2000C0604000C0606000C0608000C060A000C060C000C060E000C0800000C080
      2000C0804000C0806000C0808000C080A000C080C000C080E000C0A00000C0A0
      2000C0A04000C0A06000C0A08000C0A0A000C0A0C000C0A0E000C0C00000C0C0
      2000C0C04000C0C06000C0C08000C0C0A000F0FBFF00A4A0A000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF0007070707071D
      1D1D1D1D1D0707070707070707271E1F2727271F1E1D1D07070707071E276FB7
      F6F6F6BF6F271E1D0707072727B7F6FFF6F6F6FFFFBF271E1D070727B7FFF6B7
      6FBF276FF6FFBF271D07276FF6FF6F27F6FF6F1F27F6FF6F1E1D27B7FFBF6FF6
      FFF6B7271F6FFFBF1F1D27B7FFB7BFFFAF6FF6672727F6F6271E67BFFFB76F6F
      2727B7B72727F6F6271E6F08FFB76F6F6F676FB72727F6F6271E67BFFFF66F6F
      6F6F676F6FB7FFB7271D076FF6FFBF6F6F6F6727B7F6F66F1E07076FF6F6FFF6
      B7B7B7BFFFFFB7271E070707AFF6FFFFFFFFFFFFF6B7271F07070707076FBFF6
      F6F6F6B76F272707070707070707076FAFAF6F67270707070707}
    Glyphs.PostHint = 'Post'
    Glyphs.CancelButton.Data = {
      36050000424D3605000000000000360400002800000010000000100000000100
      0800000000000001000000000000000000000001000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000C0DCC000F0CA
      A6000020400000206000002080000020A0000020C0000020E000004000000040
      20000040400000406000004080000040A0000040C0000040E000006000000060
      20000060400000606000006080000060A0000060C0000060E000008000000080
      20000080400000806000008080000080A0000080C0000080E00000A0000000A0
      200000A0400000A0600000A0800000A0A00000A0C00000A0E00000C0000000C0
      200000C0400000C0600000C0800000C0A00000C0C00000C0E00000E0000000E0
      200000E0400000E0600000E0800000E0A00000E0C00000E0E000400000004000
      20004000400040006000400080004000A0004000C0004000E000402000004020
      20004020400040206000402080004020A0004020C0004020E000404000004040
      20004040400040406000404080004040A0004040C0004040E000406000004060
      20004060400040606000406080004060A0004060C0004060E000408000004080
      20004080400040806000408080004080A0004080C0004080E00040A0000040A0
      200040A0400040A0600040A0800040A0A00040A0C00040A0E00040C0000040C0
      200040C0400040C0600040C0800040C0A00040C0C00040C0E00040E0000040E0
      200040E0400040E0600040E0800040E0A00040E0C00040E0E000800000008000
      20008000400080006000800080008000A0008000C0008000E000802000008020
      20008020400080206000802080008020A0008020C0008020E000804000008040
      20008040400080406000804080008040A0008040C0008040E000806000008060
      20008060400080606000806080008060A0008060C0008060E000808000008080
      20008080400080806000808080008080A0008080C0008080E00080A0000080A0
      200080A0400080A0600080A0800080A0A00080A0C00080A0E00080C0000080C0
      200080C0400080C0600080C0800080C0A00080C0C00080C0E00080E0000080E0
      200080E0400080E0600080E0800080E0A00080E0C00080E0E000C0000000C000
      2000C0004000C0006000C0008000C000A000C000C000C000E000C0200000C020
      2000C0204000C0206000C0208000C020A000C020C000C020E000C0400000C040
      2000C0404000C0406000C0408000C040A000C040C000C040E000C0600000C060
      2000C0604000C0606000C0608000C060A000C060C000C060E000C0800000C080
      2000C0804000C0806000C0808000C080A000C080C000C080E000C0A00000C0A0
      2000C0A04000C0A06000C0A08000C0A0A000C0A0C000C0A0E000C0C00000C0C0
      2000C0C04000C0C06000C0C08000C0C0A000F0FBFF00A4A0A000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF0007070707071D
      1D1D1D1D1D0707070707070707271E1F2727271F1E1D1D07070707071E276FB7
      F6F6F6BF6F271E1D0707072727B7F6FFF6F6F6FFFFBF271E1D070727B7FFF627
      27271F1FB7F6BF271D07276FF6FFB7B767272727B7B7FF6F1E1D27B7FFBF67B7
      F66767F6B727F6BF1F1D27B7FF6F6F67B7F6F6B72727BFF6271E67BFFF6F6F6F
      6FF6F66F2727B7F6271E6F08FF6F6F6FF6B7B7F66F27F6F6271E67BFFFF66FF6
      B76F6FB7F66FF6B7271D076FF6FFB7AF6F6F6F6FB7F6F66F1E07076FF6F6FFF6
      6F6F6FB7FFFFB7271E070707AFF6FFFFFFFFFFFFF6B7271F07070707076FBFF6
      F6F6F6B76F272707070707070707076FAFAF6F67270707070707}
    Glyphs.CancelHint = 'Cancel'
    HoverColor = clNone
    HoverFontColor = clNone
    HeaderStyle = hsRaised
    ID = 0
    Indicators.Browse.Data = {
      F6000000424DF600000000000000760000002800000010000000100000000100
      0400000000008000000000000000000000001000000000000000000000000000
      8000008000000080800080000000800080008080000080808000C0C0C0000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00888888888888
      8888888888888888888888880888888888888888008888888888888800088888
      8888888800008888888888880000088888888888000000888888888800000008
      8888888800000088888888880000088888888888000088888888888800088888
      8888888800888888888888880888888888888888888888888888}
    Indicators.Insert.Data = {
      F6000000424DF600000000000000760000002800000010000000100000000100
      0400000000008000000000000000000000001000000000000000000000000000
      8000008000000080800080000000800080008080000080808000C0C0C0000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00888888888888
      8888888888888888888888888888888888888888888888888888888888088888
      8888888888088888888888800808008888888888800088888888888880008888
      8888888008080088888888888808888888888888880888888888888888888888
      8888888888888888888888888888888888888888888888888888}
    Indicators.Edit.Data = {
      F6000000424DF600000000000000760000002800000010000000100000000100
      0400000000008000000000000000000000001000000000000000000000000000
      8000008000000080800080000000800080008080000080808000C0C0C0000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00888888888888
      8888888888888888888888888888888888888880080088888888888880888888
      8888888880888888888888888088888888888888808888888888888880888888
      8888888880888888888888888088888888888888808888888888888880888888
      8888888008008888888888888888888888888888888888888888}
    MouseSelect = msEdit
    Nodes.NodeOpen.Stretch = False
    Nodes.NodeOpen.Frame = 1
    Nodes.NodeOpen.Data = {
      424DBE0000000000000076000000280000000900000009000000010004000000
      000048000000120B0000120B0000100000001000000000000000000080000080
      00000080800080000000800080008080000080808000C0C0C0000000FF0000FF
      000000FFFF00FF000000FF00FF00FFFF0000FFFFFF0000000000000000000FFF
      FFFF000000000FFF0FFF000000000FFF0FFF000000000F00000F000000000FFF
      0FFF000000000FFF0FFF000000000FFFFFFF000000000000000000000000}
    Nodes.NodeClosed.Stretch = False
    Nodes.NodeClosed.Frame = 1
    Nodes.NodeClosed.Data = {
      424DBE0000000000000076000000280000000900000009000000010004000000
      000048000000120B0000120B0000100000001000000000000000000080000080
      00000080800080000000800080008080000080808000C0C0C0000000FF0000FF
      000000FFFF00FF000000FF00FF00FFFF0000FFFFFF0000000000000000000FFF
      FFFF000000000FFFFFFF000000000FFFFFFF000000000F00000F000000000FFF
      FFFF000000000FFFFFFF000000000FFFFFFF000000000000000000000000}
    OuterBorder.Show = False
    OuterBorder.Color = clWebBLACK
    Page = 0
    RowCount = 5
    RowHeader.Show = False
    RowHeader.Width = 40
    RowHeader.Borders.Inner = ibAll
    RowHeader.Borders.Outer = obAll
    RowHeader.Borders.Padding = 0
    RowHeader.Borders.Spacing = 0
    RowHeader.Borders.Width = 1
    RowHeader.Borders.Collapsed = True
    RowHeader.Borders.Color = clWebLIGHTBLUE
    RowHeader.Borders.ColorDark = clNone
    RowHeader.Borders.ColorLight = clNone
    RowHeader.Color = clWebLIGHTBLUE
    RowHeader.Gradient1 = clNone
    RowHeader.Gradient2 = clNone
    RowHeader.GradientDirection = gdHorizontal
    Scroll.Style = scNever
    Scroll.Scrollbar3DLightColor = clNone
    Scroll.ScrollbarArrowColor = clNone
    Scroll.ScrollbarBaseColor = clNone
    Scroll.ScrollbarTrackColor = clNone
    Scroll.ScrollbarDarkshadowColor = clNone
    Scroll.ScrollbarFaceColor = clNone
    Scroll.ScrollbarHighlightColor = clNone
    Scroll.ScrollbarShadowColor = clNone
    SelectColor = clHighlight
    SelectFontColor = clHighlightText
    ShowColumnHeader = True
    ShowFooter = False
    ShowSelect = True
    SortSettings.Show = False
    SortSettings.Column = 0
    SortSettings.Direction = sdAscending
    SortSettings.InitSortDir = sdAscending
    StretchColumn = -1
    TabOrder = 4
    UseFullHeight = True
    ActiveRow = 0
    AlwaysEdit = False
    TotalRows = 5
  end
  object IWLabel1: TIWLabel
    Left = 606
    Top = 272
    Width = 259
    Height = 19
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
    Caption = #39'create report'#39' button at top of page.'
    RawText = False
  end
  object InvalidDateLabel: TIWLabel
    Left = 154
    Top = 120
    Width = 111
    Height = 19
    Cursor = crAuto
    Visible = False
    IW50Hint = False
    ParentShowHint = False
    ShowHint = True
    ZIndex = 0
    RenderSize = False
    Alignment = taLeftJustify
    BGColor = clNone
    Font.Color = clWebRED
    Font.FontName = 'Arial'
    Font.Size = 10
    Font.Style = [fsBold]
    NoWrap = False
    FriendlyName = 'IWLabel1'
    Caption = '(invalid date)'
    RawText = False
  end
  object BackButton: TIWLink
    Left = 163
    Top = 11
    Width = 46
    Height = 17
    Cursor = crAuto
    IW50Hint = False
    ParentShowHint = False
    ShowHint = True
    ZIndex = 0
    RenderSize = False
    Caption = 'Back'
    Color = clNone
    Font.Color = clWebBLUE
    Font.FontName = 'Arial'
    Font.Size = 10
    Font.Style = [fsUnderline]
    ScriptEvents = <>
    DoSubmitValidation = False
    FriendlyName = 'DeleteButton'
    OnClick = BackButtonClick
    TabOrder = 23
    RawText = False
  end
  object BackImage: TIWImageFile
    Left = 136
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
    ImageFile.Filename = 'd:\development\FarmWeb\Source\Files\nav_left_green.gif'
  end
  object IWImageFile1: TIWImageFile
    Left = 216
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
    Left = 243
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
  object IWImageFile3: TIWImageFile
    Left = 296
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
    ImageFile.Filename = 'd:\development\FarmWeb\Source\Files\preferences.gif'
  end
  object SetupButton: TIWLink
    Left = 323
    Top = 11
    Width = 102
    Height = 17
    Cursor = crAuto
    IW50Hint = False
    ParentShowHint = False
    ShowHint = True
    ZIndex = 0
    RenderSize = False
    Caption = 'Setting up...'
    Color = clNone
    Font.Color = clWebBLUE
    Font.FontName = 'Arial'
    Font.Size = 10
    Font.Style = [fsUnderline]
    ScriptEvents = <>
    DoSubmitValidation = False
    FriendlyName = 'SaveButton'
    OnClick = SetupButtonClick
    TabOrder = 23
    RawText = False
  end
  object RainfallButton: TIWLink
    Left = 443
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
    Left = 416
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
    Left = 736
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
    Left = 763
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
  object IWLabel4: TIWLabel
    Left = 584
    Top = 72
    Width = 356
    Height = 19
    Cursor = crAuto
    IW50Hint = False
    ParentShowHint = False
    ShowHint = True
    ZIndex = 0
    RenderSize = False
    Alignment = taLeftJustify
    BGColor = clNone
    Font.Color = clWebCRIMSON
    Font.FontName = 'Arial'
    Font.Size = 10
    Font.Style = [fsBold]
    NoWrap = False
    FriendlyName = 'IWLabel1'
    Caption = 'NEW: Nitrogen comparison report allows'
    RawText = False
  end
  object IWLabel5: TIWLabel
    Left = 606
    Top = 96
    Width = 276
    Height = 19
    Cursor = crAuto
    IW50Hint = False
    ParentShowHint = False
    ShowHint = True
    ZIndex = 0
    RenderSize = False
    Alignment = taRightJustify
    BGColor = clNone
    Font.Color = clWebCRIMSON
    Font.FontName = 'Arial'
    Font.Size = 10
    Font.Style = [fsBold]
    NoWrap = False
    FriendlyName = 'IWLabel1'
    Caption = 'you to compare 3 different fertiliser'
    RawText = False
  end
  object IWLabel8: TIWLabel
    Left = 606
    Top = 120
    Width = 270
    Height = 19
    Cursor = crAuto
    IW50Hint = False
    ParentShowHint = False
    ShowHint = True
    ZIndex = 0
    RenderSize = False
    Alignment = taRightJustify
    BGColor = clNone
    Font.Color = clWebCRIMSON
    Font.FontName = 'Arial'
    Font.Size = 10
    Font.Style = [fsBold]
    NoWrap = False
    FriendlyName = 'IWLabel1'
    Caption = 'scenarios. This is especially useful'
    RawText = False
  end
  object HelpFileLink: TIWLink
    Left = 608
    Top = 168
    Width = 289
    Height = 17
    Cursor = crAuto
    IW50Hint = False
    ParentShowHint = False
    ShowHint = True
    ZIndex = 0
    RenderSize = False
    Caption = 'Please read the help file'
    Color = clNone
    Font.Color = clWebCRIMSON
    Font.FontName = 'Arial'
    Font.Size = 10
    Font.Style = [fsBold, fsUnderline]
    ScriptEvents = <>
    DoSubmitValidation = False
    FriendlyName = 'HelpFileLink'
    OnClick = HelpFileLinkClick
    TabOrder = 31
    RawText = False
  end
  object IWLabel10: TIWLabel
    Left = 606
    Top = 144
    Width = 281
    Height = 19
    Cursor = crAuto
    IW50Hint = False
    ParentShowHint = False
    ShowHint = True
    ZIndex = 0
    RenderSize = False
    Alignment = taRightJustify
    BGColor = clNone
    Font.Color = clWebCRIMSON
    Font.FontName = 'Arial'
    Font.Size = 10
    Font.Style = [fsBold]
    NoWrap = False
    FriendlyName = 'IWLabel1'
    Caption = 'for comparing top dressing N rates.'
    RawText = False
  end
  object IWLabel12: TIWLabel
    Left = 608
    Top = 192
    Width = 291
    Height = 19
    Cursor = crAuto
    IW50Hint = False
    ParentShowHint = False
    ShowHint = True
    ZIndex = 0
    RenderSize = False
    Alignment = taRightJustify
    BGColor = clNone
    Font.Color = clWebCRIMSON
    Font.FontName = 'Arial'
    Font.Size = 10
    Font.Style = [fsBold]
    NoWrap = False
    FriendlyName = 'IWLabel1'
    Caption = 'on how to do a N comparison report.'
    RawText = False
  end
  object EmailFilesCheckBox: TIWCheckBox
    Left = 872
    Top = 8
    Width = 121
    Height = 21
    Cursor = crAuto
    IW50Hint = False
    ParentShowHint = False
    ShowHint = True
    ZIndex = 0
    RenderSize = False
    Caption = 'Email con/par'
    Editable = True
    Font.Color = clWebBLUE
    Font.FontName = 'Arial'
    Font.Size = 10
    Font.Style = []
    ScriptEvents = <>
    DoSubmitValidation = True
    Style = stNormal
    TabOrder = 32
    Checked = False
    FriendlyName = 'EmailFilesCheckBox'
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
  object VisitorLabel1: TIWLabel
    Left = 144
    Top = 400
    Width = 849
    Height = 19
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
    Caption = 
      'Welcome to the Visitors site of the Yield Prophet.  The Visitors' +
      ' site enables you to check out how the Yield'
    RawText = False
  end
  object VisitorLabel2: TIWLabel
    Left = 144
    Top = 416
    Width = 843
    Height = 19
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
    Caption = 
      'Prophet operates and see some of the reports which were generate' +
      'd over the season.  The Visitors site is'
    RawText = False
  end
  object VisitorLabel3: TIWLabel
    Left = 144
    Top = 432
    Width = 718
    Height = 19
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
    Caption = 
      'not an active site '#8211' you cannot actually run the model, it is de' +
      'signed only to show you how'
    RawText = False
  end
  object VisitorLabel4: TIWLabel
    Left = 144
    Top = 448
    Width = 217
    Height = 19
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
    Caption = 'the Yield Prophet operates.'
    RawText = False
  end
  object VisitorLabel5: TIWLabel
    Left = 144
    Top = 480
    Width = 673
    Height = 19
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
    Caption = 
      'The Visitors site is using updated information from the BCG main' +
      ' trial site at Birchip.'
    RawText = False
  end
  object VisitorLabel6: TIWLabel
    Left = 144
    Top = 520
    Width = 716
    Height = 19
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
    Caption = 
      'The help files will help you in becoming familiar with how to us' +
      'e the Yield Prophet web site'
    RawText = False
  end
  object VisitorLabel7: TIWLabel
    Left = 144
    Top = 536
    Width = 525
    Height = 19
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
    Caption = 
      'and there are also help files explaining how to interpret the re' +
      'ports'
    RawText = False
  end
  object VisitorLabel8: TIWLabel
    Left = 144
    Top = 576
    Width = 738
    Height = 19
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
    Caption = 
      'If you have any comments or would like to find out more about th' +
      'e Yield Prophet please drop'
    RawText = False
  end
  object VisitorLabel9: TIWLabel
    Left = 144
    Top = 592
    Width = 412
    Height = 19
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
    Caption = 'a line to James Hunt at:  james.hunt@aanet.com.au'
    RawText = False
  end
end
