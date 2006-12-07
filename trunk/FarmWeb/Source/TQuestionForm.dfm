object QuestionForm: TQuestionForm
  Left = 0
  Top = 0
  Width = 555
  Height = 400
  ConnectionMode = cmAny
  SupportedBrowsers = [brIE, brNetscape7, brOpera, brNetscape6]
  BrowserSecurityCheck = True
  Background.Fixed = False
  HandleTabs = False
  LockUntilLoaded = True
  LockOnSubmit = True
  UpdateMode = umAll
  DesignLeft = 584
  DesignTop = 326
  object YesButton: TIWButton
    Left = 32
    Top = 112
    Width = 75
    Height = 25
    Cursor = crAuto
    IW50Hint = False
    ParentShowHint = False
    ShowHint = True
    ZIndex = 0
    RenderSize = True
    ButtonType = btButton
    Caption = 'Yes'
    Color = clBtnFace
    DoSubmitValidation = True
    Font.Color = clNone
    Font.Enabled = True
    Font.Size = 10
    Font.Style = []
    FriendlyName = 'YesButton'
    ScriptEvents = <>
    TabOrder = 1
    OnClick = YesButtonClick
  end
  object NoButton: TIWButton
    Left = 128
    Top = 112
    Width = 75
    Height = 25
    Cursor = crAuto
    IW50Hint = False
    ParentShowHint = False
    ShowHint = True
    ZIndex = 0
    RenderSize = True
    ButtonType = btButton
    Caption = 'No'
    Color = clBtnFace
    DoSubmitValidation = True
    Font.Color = clNone
    Font.Enabled = True
    Font.Size = 10
    Font.Style = []
    FriendlyName = 'NoButton'
    ScriptEvents = <>
    TabOrder = 2
    OnClick = NoButtonClick
  end
  object PromptLabel: TIWLabel
    Left = 32
    Top = 24
    Width = 156
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
    Font.Enabled = True
    Font.FontName = 'Arial'
    Font.Size = 10
    Font.Style = []
    NoWrap = False
    FriendlyName = 'IWLabel1'
    Caption = 'Question goes here'
    RawText = False
  end
end