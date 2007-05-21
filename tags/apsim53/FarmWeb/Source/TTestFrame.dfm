object TestFrame: TTestFrame
  Left = 0
  Top = 0
  Width = 676
  Height = 455
  TabOrder = 0
  object IWFrameRegion: TIWRegion
    Left = 0
    Top = 0
    Width = 676
    Height = 455
    Cursor = crAuto
    Align = alClient
    BorderOptions.NumericWidth = 1
    BorderOptions.BorderWidth = cbwNumeric
    BorderOptions.Style = cbsSolid
    BorderOptions.Color = clNone
    TabOrder = 0
    Color = clNone
    ParentShowHint = False
    ShowHint = True
    ZIndex = -1
    object IWLabel1: TIWLabel
      Left = 64
      Top = 64
      Width = 128
      Height = 20
      Cursor = crAuto
      IW50Hint = False
      ParentShowHint = False
      ShowHint = True
      ZIndex = 0
      RenderSize = False
      Alignment = taLeftJustify
      BGColor = clNone
      Font.Color = clNone
      Font.Size = 10
      Font.Style = []
      NoWrap = False
      FriendlyName = 'IWLabel1'
      Caption = 'Text from frame'
      RawText = False
    end
  end
end
