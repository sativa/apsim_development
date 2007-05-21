object ReportForm: TReportForm
  Left = 194
  Top = 117
  Align = alClient
  BorderIcons = []
  BorderStyle = bsNone
  ClientHeight = 458
  ClientWidth = 686
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object TabControl: TTabControl
    Left = 0
    Top = 0
    Width = 686
    Height = 458
    Align = alClient
    PopupMenu = PopupMenu
    TabOrder = 0
    TabPosition = tpBottom
    OnChange = TabControlChange
    OnDragDrop = TabControlDragDrop
    OnDragOver = TabControlDragOver
    OnMouseDown = TabControlMouseDown
    object Report: TSEGReport
      Left = 4
      Top = 4
      Width = 678
      Height = 450
      Align = alClient
      BevelInner = bvNone
      BevelOuter = bvNone
      BorderStyle = bsNone
      TabOrder = 0
      zoom = 50
      isPortrait = True
    end
  end
  object PopupMenu: TPopupMenu
    Left = 64
    Top = 392
    object CreatePage: TMenuItem
      Caption = '&Create a new page'
      OnClick = CreatePageClick
    end
    object DeletePage: TMenuItem
      Caption = '&Delete current page'
      OnClick = DeletePageClick
    end
    object RenamePage: TMenuItem
      Caption = '&Rename current page'
      OnClick = RenamePageClick
    end
  end
end
