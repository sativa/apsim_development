object ViewReportForm: TViewReportForm
  Left = 0
  Top = 0
  Width = 864
  Height = 676
  ConnectionMode = cmAny
  SupportedBrowsers = [brIE, brNetscape7, brOpera, brSafari, brNetscape6]
  BrowserSecurityCheck = True
  Background.Fixed = False
  HandleTabs = False
  LockUntilLoaded = True
  LockOnSubmit = True
  ShowHint = True
  DesignSize = (
    864
    676)
  DesignLeft = 513
  DesignTop = 202
  object IWRectangle1: TIWRectangle
    Left = 0
    Top = 0
    Width = 864
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
  object IWImageFile2: TIWImageFile
    Left = 152
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
  object BackButton: TIWLink
    Left = 179
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
  object Image: TIWImageFile
    Left = 152
    Top = 40
    Width = 705
    Height = 633
    Cursor = crAuto
    Anchors = [akLeft, akTop, akRight, akBottom]
    IW50Hint = False
    ParentShowHint = False
    ShowHint = True
    ZIndex = 0
    RenderSize = False
    BorderOptions.Color = clNone
    BorderOptions.Width = 0
    DoSubmitValidation = True
    ScriptEvents = <>
    TabOrder = 3
    UseSize = False
    FriendlyName = 'Image'
  end
end
