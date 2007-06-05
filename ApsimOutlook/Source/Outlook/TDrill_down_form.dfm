object Drill_down_form: TDrill_down_form
  Left = 261
  Top = 166
  Width = 539
  Height = 464
  Caption = 'Choose Scenarios'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 408
    Top = 0
    Width = 115
    Height = 428
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 0
    object ExpandAllLabel: TLabel
      Left = 8
      Top = 120
      Width = 49
      Height = 13
      Cursor = crHandPoint
      Caption = 'Expand all'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsUnderline]
      ParentFont = False
      OnClick = ShowAllButtonClick
    end
    object CollapseAllLabel: TLabel
      Left = 8
      Top = 144
      Width = 53
      Height = 13
      Cursor = crHandPoint
      Caption = 'Collapse all'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsUnderline]
      ParentFont = False
      OnClick = HideAllButtonClick
    end
    object ClearLabel: TLabel
      Left = 8
      Top = 168
      Width = 24
      Height = 13
      Cursor = crHandPoint
      Caption = 'Clear'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsUnderline]
      ParentFont = False
      OnClick = ClearButtonClick
    end
    object AddInLabel: TLabel
      Left = 8
      Top = 248
      Width = 92
      Height = 13
      Cursor = crHandPoint
      Caption = 'Add-in caption here'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsUnderline]
      ParentFont = False
      Visible = False
      OnClick = AddInLabelClick
    end
    object LogoImage: TImage
      Left = 0
      Top = 334
      Width = 115
      Height = 94
      Align = alBottom
      Center = True
    end
    object SaveLabel: TLabel
      Left = 8
      Top = 192
      Width = 73
      Height = 13
      Cursor = crHandPoint
      Caption = 'Save scenarios'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsUnderline]
      ParentFont = False
      OnClick = SaveLabelClick
    end
    object RestoreLabel: TLabel
      Left = 8
      Top = 216
      Width = 85
      Height = 13
      Cursor = crHandPoint
      Caption = 'Restore scenarios'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsUnderline]
      ParentFont = False
      OnClick = RestoreLabelClick
    end
    object Ok_button: TBitBtn
      Left = 7
      Top = 22
      Width = 90
      Height = 30
      Caption = 'OK'
      ModalResult = 1
      TabOrder = 0
      Glyph.Data = {
        DE010000424DDE01000000000000760000002800000024000000120000000100
        0400000000006801000000000000000000001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        3333333333333333333333330000333333333333333333333333F33333333333
        00003333344333333333333333388F3333333333000033334224333333333333
        338338F3333333330000333422224333333333333833338F3333333300003342
        222224333333333383333338F3333333000034222A22224333333338F338F333
        8F33333300003222A3A2224333333338F3838F338F33333300003A2A333A2224
        33333338F83338F338F33333000033A33333A222433333338333338F338F3333
        0000333333333A222433333333333338F338F33300003333333333A222433333
        333333338F338F33000033333333333A222433333333333338F338F300003333
        33333333A222433333333333338F338F00003333333333333A22433333333333
        3338F38F000033333333333333A223333333333333338F830000333333333333
        333A333333333333333338330000333333333333333333333333333333333333
        0000}
      NumGlyphs = 2
    end
    object Cancel_button: TBitBtn
      Left = 7
      Top = 62
      Width = 90
      Height = 30
      TabOrder = 1
      Kind = bkCancel
    end
    object AddInBevel: TPanel
      Left = 8
      Top = 240
      Width = 100
      Height = 2
      TabOrder = 2
      Visible = False
    end
  end
  object ScenarioTree: TTreeView
    Left = 0
    Top = 0
    Width = 408
    Height = 428
    Align = alClient
    DragMode = dmAutomatic
    HotTrack = True
    Images = SmallImageList
    Indent = 19
    TabOrder = 1
    OnCollapsing = ScenarioTreeCollapsing
    OnDragDrop = ScenarioTreeDragDrop
    OnDragOver = ScenarioTreeDragOver
    OnEdited = ScenarioTreeEdited
    OnEditing = ScenarioTreeEditing
    OnExpanding = ScenarioTreeExpanding
    OnMouseDown = ScenarioTreeMouseDown
  end
  object SmallImageList: TImageList
    BkColor = clWhite
    Left = 424
    Top = 304
  end
  object ScenarioNamePopup: TPopupMenu
    Left = 456
    Top = 304
    object Rename1: TMenuItem
      Caption = '&Rename'
      OnClick = Rename1Click
    end
    object Delete1: TMenuItem
      Caption = '&Delete'
      OnClick = Delete1Click
    end
  end
end
