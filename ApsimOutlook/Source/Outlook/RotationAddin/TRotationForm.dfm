object RotationForm: TRotationForm
  Left = 247
  Top = 156
  Width = 303
  Height = 407
  BorderIcons = []
  Caption = 'Rotations'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ListBox: TSectionListBox
    Left = 0
    Top = 81
    Width = 295
    Height = 251
    Sections = <
      item
        Alignment = taLeftJustify
        VAlignment = vtaCenter
        AutoEdit = False
        AutoSize = True
        Caption = 'Unknown files'
        Fixed = False
        ImageIndex = -1
        State = lssExpanded
        SubItems.Strings = (
          'item1'
          'item2')
        Color = clWindow
        Lines = slNone
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        FontUsage = fuSubItems
        EndEllipsis = False
        ControlType = scText
        RadioIndex = 0
        SortDirection = sdNone
        SortShow = False
        Tag = 0
        OwnerDraw = False
        ReadOnly = True
        ItemHeight = 16
      end
      item
        Alignment = taLeftJustify
        VAlignment = vtaCenter
        AutoEdit = False
        AutoSize = True
        Caption = 'Rotation1'
        Fixed = False
        ImageIndex = -1
        State = lssExpanded
        SubItems.Strings = (
          'item3'
          'item4')
        Color = clWindow
        Lines = slNone
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        FontUsage = fuSubItems
        EndEllipsis = False
        ControlType = scText
        RadioIndex = 0
        SortDirection = sdNone
        SortShow = False
        Tag = 0
        OwnerDraw = False
        ReadOnly = True
        ItemHeight = 16
      end>
    TabPositions = <>
    TabPosMove = False
    Align = alClient
    WordWrap = False
    OneExpanded = False
    FullFocus = True
    DragMode = dmAutomatic
    ImageSpacing = 16
    MultiSelect = True
    SubItemHeight = 16
    TabOrder = 0
    OnDragDrop = ListBoxDragDrop
    OnDragOver = ListBoxDragOver
    OnMouseUp = ListBoxMouseUp
    Flat = True
    SectionHeight = 20
    SectionFont.Charset = DEFAULT_CHARSET
    SectionFont.Color = clWindowText
    SectionFont.Height = -11
    SectionFont.Name = 'MS Sans Serif'
    SectionFont.Style = []
    SectionIndent = 14
    SubItemImages = ImageList1
    SubItemIndent = 14
    URLSettings.URLAware = False
    URLSettings.URLColor = clBlue
    URLSettings.URLFull = False
    ExpandDisable = False
    ContractDisable = False
    SectionColor = clBtnFace
    ActiveSection = asFull
    NodeType = lnFlat
    SectionFocus = sfDash
    ScrollStyle = slsNormal
    ScrollColor = clNone
    ScrollWidth = 17
    SelectionColor = clHighlight
    SelectionTextColor = clHighlightText
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 295
    Height = 81
    Align = alTop
    BevelInner = bvLowered
    BevelOuter = bvNone
    TabOrder = 1
    object Label1: TLabel
      Left = 12
      Top = 24
      Width = 121
      Height = 13
      Caption = 'Enter number of rotations:'
    end
    object Label2: TLabel
      Left = 52
      Top = 51
      Width = 189
      Height = 13
      Caption = 'Files can be dragged between rotations.'
    end
    object Label3: TLabel
      Left = 22
      Top = 67
      Width = 246
      Height = 13
      Caption = 'Right click on a rotation name to rename or delete it.'
    end
    object RotationCheck: TCheckBox
      Left = 32
      Top = 8
      Width = 121
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Do rotation analysis?'
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
    object NumRotationsEdit: TEdit
      Left = 140
      Top = 24
      Width = 25
      Height = 21
      ReadOnly = True
      TabOrder = 1
      Text = '1'
      OnChange = NumRotationsEditChange
    end
    object NumRotationsUpDown: TUpDown
      Left = 165
      Top = 24
      Width = 16
      Height = 21
      Associate = NumRotationsEdit
      Min = 1
      Position = 1
      TabOrder = 2
      Wrap = False
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 332
    Width = 295
    Height = 41
    Align = alBottom
    BevelInner = bvLowered
    BevelOuter = bvNone
    TabOrder = 2
    object BitBtn1: TBitBtn
      Left = 72
      Top = 8
      Width = 75
      Height = 25
      TabOrder = 0
      Kind = bkOK
    end
    object BitBtn2: TBitBtn
      Left = 152
      Top = 8
      Width = 75
      Height = 25
      TabOrder = 1
      Kind = bkCancel
    end
  end
  object ImageList1: TImageList
    Left = 144
    Top = 192
    Bitmap = {
      494C010101000400040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000C0C0C000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000C0C0C0000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C0C0C0000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C0C0C0000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C0C0C0000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C0C0C0000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C0C0C0000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C0C0C0000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C0C0C0000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C0C0C0000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C0C0C0000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C0C0C0000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C0C0C0000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000808080000000000080808000000000008080800000000000808080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00C0070000000000008003000000000000
      BFF3000000000000BFF3000000000000B033000000000000BFF3000000000000
      B033000000000000BFF3000000000000B033000000000000BFF3000000000000
      B133000000000000BFF3000000000000BFF3000000000000AAAB000000000000
      D557000000000000EAAF00000000000000000000000000000000000000000000
      000000000000}
  end
  object PopupMenu1: TPopupMenu
    Left = 176
    Top = 192
    object RenameMenuItem: TMenuItem
      Caption = '&Rename'
      OnClick = RenameMenuItemClick
    end
    object DeleteMenuItem: TMenuItem
      Caption = '&Delete'
      OnClick = DeleteMenuItemClick
    end
  end
end