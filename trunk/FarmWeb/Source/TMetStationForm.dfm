object MetStationForm: TMetStationForm
  Left = 0
  Top = 0
  Width = 765
  Height = 671
  ConnectionMode = cmAny
  Title = 'Yield Prophet'
  SupportedBrowsers = [brIE, brNetscape7, brOpera, brNetscape6]
  BrowserSecurityCheck = True
  Background.Fixed = False
  HandleTabs = False
  LockUntilLoaded = True
  LockOnSubmit = True
  ShowHint = True
  DesignSize = (
    765
    671)
  DesignLeft = 559
  DesignTop = 175
  object IWRectangle1: TIWRectangle
    Left = 0
    Top = 0
    Width = 765
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
  object PromptLabel: TIWLabel
    Left = 144
    Top = 40
    Width = 61
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
    FriendlyName = 'PromptLabel'
    Caption = 'Region:'
    RawText = False
  end
  object RegionCombo: TIWComboBox
    Left = 144
    Top = 64
    Width = 609
    Height = 21
    Cursor = crAuto
    Anchors = [akLeft, akTop, akRight]
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
    OnChange = RegionComboChange
    UseSize = False
    DoSubmitValidation = True
    Editable = True
    NonEditableAsLabel = True
    TabOrder = 4
    ItemIndex = -1
    Sorted = False
    FriendlyName = 'RegionCombo'
  end
  object MetStationList: TIWListbox
    Left = 144
    Top = 112
    Width = 609
    Height = 513
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
    RequireSelection = False
    ScriptEvents = <>
    UseSize = True
    DoSubmitValidation = True
    Editable = True
    TabOrder = 7
    FriendlyName = 'MetStationList'
    ItemIndex = -1
    MultiSelect = True
    Sorted = False
  end
  object IWLabel1: TIWLabel
    Left = 144
    Top = 88
    Width = 100
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
    FriendlyName = 'PromptLabel'
    Caption = 'Met stations:'
    RawText = False
  end
  object ImportFile: TIWFile
    Left = 568
    Top = 8
    Width = 193
    Height = 25
    Cursor = crAuto
    IW50Hint = False
    ParentShowHint = False
    ShowHint = True
    ZIndex = 0
    RenderSize = True
    TabOrder = 10
    FriendlyName = 'ImportFile'
    Font.Color = clNone
    Font.Size = 10
    Font.Style = []
    BGColor = clNone
    ButtonHeight = 21
    FrameBGColor = clNone
  end
  object IWImageFile2: TIWImageFile
    Left = 144
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
    ImageFile.Filename = 'C:\MyData\development\FarmWeb\Source\Files\add2.gif'
  end
  object AddRegionButton: TIWLink
    Left = 171
    Top = 11
    Width = 94
    Height = 17
    Cursor = crAuto
    IW50Hint = False
    ParentShowHint = False
    ShowHint = True
    ZIndex = 0
    RenderSize = False
    Caption = 'Add region'
    Color = clNone
    Font.Color = clWebBLUE
    Font.FontName = 'Arial'
    Font.Size = 10
    Font.Style = [fsUnderline]
    ScriptEvents = <>
    DoSubmitValidation = False
    FriendlyName = 'AddRegionButton'
    OnClick = AddRegionButtonClick
    TabOrder = 23
    RawText = False
  end
  object IWImageFile3: TIWImageFile
    Left = 264
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
    ImageFile.Filename = 'C:\MyData\development\FarmWeb\Source\Files\delete2.gif'
  end
  object DeleteRegionButton: TIWLink
    Left = 291
    Top = 11
    Width = 110
    Height = 17
    Cursor = crAuto
    IW50Hint = False
    ParentShowHint = False
    ShowHint = True
    ZIndex = 0
    RenderSize = False
    Caption = 'Delete region'
    Color = clNone
    Font.Color = clWebBLUE
    Font.FontName = 'Arial'
    Font.Size = 10
    Font.Style = [fsUnderline]
    ScriptEvents = <>
    DoSubmitValidation = False
    FriendlyName = 'AddTypeButton'
    OnClick = DeleteRegionButtonClick
    TabOrder = 23
    RawText = False
  end
  object DeleteMetStationButton: TIWLink
    Left = 427
    Top = 11
    Width = 110
    Height = 17
    Cursor = crAuto
    IW50Hint = False
    ParentShowHint = False
    ShowHint = True
    ZIndex = 0
    RenderSize = False
    Caption = 'Delete'
    Color = clNone
    Font.Color = clWebBLUE
    Font.FontName = 'Arial'
    Font.Size = 10
    Font.Style = [fsUnderline]
    ScriptEvents = <>
    DoSubmitValidation = False
    FriendlyName = 'AddTypeButton'
    OnClick = DeleteMetStationButtonClick
    TabOrder = 23
    RawText = False
  end
  object IWImageFile1: TIWImageFile
    Left = 400
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
    ImageFile.Filename = 'C:\MyData\development\FarmWeb\Source\Files\delete2.gif'
  end
  object ImportButton: TIWLink
    Left = 515
    Top = 11
    Width = 54
    Height = 17
    Cursor = crAuto
    IW50Hint = False
    ParentShowHint = False
    ShowHint = True
    ZIndex = 0
    RenderSize = False
    Caption = 'Import'
    Color = clNone
    Font.Color = clWebBLUE
    Font.FontName = 'Arial'
    Font.Size = 10
    Font.Style = [fsUnderline]
    ScriptEvents = <>
    DoSubmitValidation = False
    FriendlyName = 'AddTypeButton'
    OnClick = ImportButtonClick
    TabOrder = 23
    RawText = False
  end
  object IWImageFile4: TIWImageFile
    Left = 488
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
    ImageFile.Filename = 'C:\MyData\development\FarmWeb\Source\Files\import1.gif'
  end
end
