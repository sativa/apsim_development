object UserDetailsForm: TUserDetailsForm
  Left = 0
  Top = 0
  Width = 696
  Height = 508
  ConnectionMode = cmAny
  Title = 'Yield Prophet'
  SupportedBrowsers = [brIE, brNetscape7, brOpera, brNetscape6]
  BrowserSecurityCheck = True
  Background.Fixed = False
  HandleTabs = False
  LockUntilLoaded = True
  LockOnSubmit = True
  ShowHint = True
  DesignLeft = 377
  DesignTop = 137
  object IWLabel5: TIWLabel
    Left = 146
    Top = 72
    Width = 47
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
    Caption = 'Name:'
    RawText = False
  end
  object NameEdit: TIWEdit
    Left = 216
    Top = 72
    Width = 281
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
    FriendlyName = 'NameEdit'
    MaxLength = 0
    ReadOnly = False
    Required = False
    ScriptEvents = <>
    TabOrder = 3
    PasswordPrompt = False
  end
  object EmailEdit: TIWEdit
    Left = 216
    Top = 104
    Width = 281
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
    FriendlyName = 'IWEdit1'
    MaxLength = 0
    ReadOnly = False
    Required = False
    ScriptEvents = <>
    TabOrder = 3
    PasswordPrompt = False
  end
  object IWLabel1: TIWLabel
    Left = 146
    Top = 104
    Width = 45
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
    Caption = 'Email:'
    RawText = False
  end
  object PasswordButton: TIWButton
    Left = 216
    Top = 136
    Width = 169
    Height = 25
    Cursor = crAuto
    IW50Hint = False
    ParentShowHint = False
    ShowHint = True
    ZIndex = 0
    RenderSize = True
    Caption = 'Change password'
    DoSubmitValidation = True
    Color = clBtnFace
    Font.Color = clNone
    Font.Size = 10
    Font.Style = []
    FriendlyName = 'PasswordButton'
    ScriptEvents = <>
    TabOrder = 0
    OnClick = PasswordButtonClick
  end
  object IWRectangle1: TIWRectangle
    Left = 0
    Top = 0
    Width = 696
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
  object SaveButton: TIWLink
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
  object IWImageFile1: TIWImageFile
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
    ImageFile.Filename = 'd:\development\FarmWeb\Source\Files\disk_blue.gif'
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
end
