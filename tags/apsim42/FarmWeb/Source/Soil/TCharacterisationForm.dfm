object CharacterisationForm: TCharacterisationForm
  Left = 738
  Top = 306
  Anchors = [akLeft, akTop, akRight, akBottom]
  BorderIcons = []
  BorderStyle = bsNone
  ClientHeight = 545
  ClientWidth = 600
  Color = clBtnFace
  TransparentColorValue = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Arial'
  Font.Style = []
  Icon.Data = {
    0000010001002020100000000000E80200001600000028000000200000004000
    0000010004000000000080020000000000000000000000000000000000000000
    000000008000008000000080800080000000800080008080000080808000C0C0
    C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF007777
    7777777777770777777777777777777777777777777707777777777777777777
    7777777707770777777777777777777777777777077707700777777777777777
    7777777707770700777777777777777777770777707707077700777777777777
    7777707770770077700777777777777777777077700700777077777777777777
    7777700077070777707777777777777777777770070707777077777777777777
    7777777707000777007777777777777777777777007077700777777777777777
    7777777770007000777777777777777777777777777707777777777777777777
    7777777777770777777777777777777777777777777707777777777777777777
    7777777777700777777777777777777777777777777700777777777777788888
    8888887777770077777777888888888888888888888007777888888888888888
    8888888888822888888888888888888888888888888228888888888888888888
    8888888828822828888888888888888888888888822222288888888888888888
    8888888888822888888888888888888888888888822228888888888888888888
    8888888888222228888888888888888888888888888228888888888888888888
    88888888888B98888888888888888888888888888889B8888888888888888888
    88888888888BB888888888888888888888888888888B98888888888888880000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000}
  KeyPreview = True
  OldCreateOrder = False
  Position = poDefault
  Scaled = False
  Visible = True
  OnClose = FormClose
  DesignSize = (
    600
    545)
  PixelsPerInch = 96
  TextHeight = 16
  object Panel2: TPanel
    Left = 8
    Top = 8
    Width = 584
    Height = 532
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    TabOrder = 0
    object PageControl: TPageControl
      Left = 0
      Top = 0
      Width = 584
      Height = 532
      ActivePage = WaterSheet
      Align = alClient
      MultiLine = True
      TabIndex = 1
      TabOrder = 0
      OnChange = PageControlChange
      OnChanging = PageControlChanging
      object GeneralSheet: TTabSheet
        Caption = 'General'
        ImageIndex = 3
        object GeneralGrid: TAdvStringGrid
          Left = 0
          Top = 0
          Width = 576
          Height = 501
          Cursor = crDefault
          Align = alClient
          BorderStyle = bsNone
          ColCount = 2
          Ctl3D = False
          DefaultColWidth = 45
          DefaultRowHeight = 21
          DefaultDrawing = False
          FixedColor = clActiveBorder
          FixedCols = 1
          RowCount = 11
          FixedRows = 0
          Font.Charset = ANSI_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'Arial'
          Font.Style = []
          GridLineWidth = 1
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing]
          ParentCtl3D = False
          ParentFont = False
          TabOrder = 0
          OnKeyDown = GridKeyDown
          OnMouseUp = GridMouseUp
          OnSetEditText = GridSetEditText
          GridLineColor = clSilver
          ActiveCellShow = False
          ActiveCellFont.Charset = DEFAULT_CHARSET
          ActiveCellFont.Color = clWhite
          ActiveCellFont.Height = -11
          ActiveCellFont.Name = 'MS Sans Serif'
          ActiveCellFont.Style = [fsBold]
          ActiveCellColor = clGray
          Bands.PrimaryColor = clInfoBk
          Bands.PrimaryLength = 1
          Bands.SecondaryColor = clWindow
          Bands.SecondaryLength = 1
          Bands.Print = False
          AutoNumAlign = False
          AutoSize = False
          VAlignment = vtaTop
          EnhTextSize = False
          EnhRowColMove = False
          SizeWithForm = False
          Multilinecells = True
          OnCanEditCell = GridCanEditCell
          DragDropSettings.OleAcceptFiles = True
          DragDropSettings.OleAcceptText = True
          SortSettings.AutoColumnMerge = False
          SortSettings.Column = 0
          SortSettings.Show = False
          SortSettings.IndexShow = False
          SortSettings.IndexColor = clYellow
          SortSettings.Full = True
          SortSettings.SingleColumn = False
          SortSettings.IgnoreBlanks = False
          SortSettings.BlankPos = blFirst
          SortSettings.AutoFormat = True
          SortSettings.Direction = sdAscending
          SortSettings.FixedCols = False
          SortSettings.NormalCellsOnly = False
          SortSettings.Row = 0
          FloatingFooter.Color = clBtnFace
          FloatingFooter.Column = 0
          FloatingFooter.FooterStyle = fsFixedLastRow
          FloatingFooter.Visible = False
          ControlLook.Color = clSilver
          ControlLook.CheckSize = 15
          ControlLook.RadioSize = 10
          ControlLook.ControlStyle = csClassic
          ControlLook.FlatButton = False
          EnableBlink = False
          EnableHTML = True
          EnableWheel = True
          Flat = True
          HintColor = clInfoBk
          SelectionColor = clMenuHighlight
          SelectionTextColor = clHighlightText
          SelectionRectangle = False
          SelectionResizer = False
          SelectionRTFKeep = False
          HintShowCells = False
          HintShowLargeText = False
          HintShowSizing = False
          PrintSettings.FooterSize = 0
          PrintSettings.HeaderSize = 0
          PrintSettings.Time = ppNone
          PrintSettings.Date = ppNone
          PrintSettings.DateFormat = 'dd/mm/yyyy'
          PrintSettings.PageNr = ppNone
          PrintSettings.Title = ppNone
          PrintSettings.Font.Charset = DEFAULT_CHARSET
          PrintSettings.Font.Color = clWindowText
          PrintSettings.Font.Height = -11
          PrintSettings.Font.Name = 'MS Sans Serif'
          PrintSettings.Font.Style = []
          PrintSettings.HeaderFont.Charset = DEFAULT_CHARSET
          PrintSettings.HeaderFont.Color = clWindowText
          PrintSettings.HeaderFont.Height = -11
          PrintSettings.HeaderFont.Name = 'MS Sans Serif'
          PrintSettings.HeaderFont.Style = []
          PrintSettings.FooterFont.Charset = DEFAULT_CHARSET
          PrintSettings.FooterFont.Color = clWindowText
          PrintSettings.FooterFont.Height = -11
          PrintSettings.FooterFont.Name = 'MS Sans Serif'
          PrintSettings.FooterFont.Style = []
          PrintSettings.Borders = pbNoborder
          PrintSettings.BorderStyle = psSolid
          PrintSettings.Centered = False
          PrintSettings.RepeatFixedRows = False
          PrintSettings.RepeatFixedCols = False
          PrintSettings.LeftSize = 0
          PrintSettings.RightSize = 0
          PrintSettings.ColumnSpacing = 0
          PrintSettings.RowSpacing = 0
          PrintSettings.TitleSpacing = 0
          PrintSettings.Orientation = poPortrait
          PrintSettings.PageNumberOffset = 0
          PrintSettings.MaxPagesOffset = 0
          PrintSettings.FixedWidth = 0
          PrintSettings.FixedHeight = 0
          PrintSettings.UseFixedHeight = False
          PrintSettings.UseFixedWidth = False
          PrintSettings.FitToPage = fpNever
          PrintSettings.PageNumSep = '/'
          PrintSettings.NoAutoSize = False
          PrintSettings.NoAutoSizeRow = False
          PrintSettings.PrintGraphics = False
          HTMLSettings.Width = 100
          HTMLSettings.XHTML = False
          Navigation.AdvanceOnEnter = True
          Navigation.AdvanceDirection = adTopBottom
          Navigation.AllowClipboardShortCuts = True
          Navigation.AllowSmartClipboard = True
          Navigation.AllowClipboardAlways = True
          Navigation.InsertPosition = pInsertBefore
          Navigation.HomeEndKey = heFirstLastColumn
          Navigation.TabToNextAtEnd = False
          Navigation.TabAdvanceDirection = adLeftRight
          ColumnSize.Stretch = True
          ColumnSize.StretchColumn = 1
          ColumnSize.Location = clRegistry
          CellNode.Color = clSilver
          CellNode.NodeColor = clBlack
          CellNode.ShowTree = False
          MaxEditLength = 0
          MouseActions.DirectComboDrop = True
          MouseActions.DisjunctCellSelect = True
          MouseActions.RangeSelectAndEdit = True
          IntelliPan = ipVertical
          URLColor = clBlue
          URLShow = False
          URLFull = False
          URLEdit = False
          ScrollType = ssNormal
          ScrollColor = clWindow
          ScrollWidth = 17
          ScrollSynch = False
          ScrollProportional = False
          ScrollHints = shNone
          OemConvert = False
          FixedFooters = 0
          FixedRightCols = 0
          FixedColWidth = 146
          FixedRowHeight = 21
          FixedFont.Charset = ANSI_CHARSET
          FixedFont.Color = clWindowText
          FixedFont.Height = -13
          FixedFont.Name = 'Arial'
          FixedFont.Style = []
          FixedAsButtons = False
          FloatFormat = '%.2f'
          IntegralHeight = False
          WordWrap = False
          ColumnHeaders.Strings = (
            'Depth\n(cm)'
            'BD\n(g/cc)'
            'DUL\n(%vol)'
            'SAT\n(%vol)'
            '')
          RowHeaders.Strings = (
            'Region:'
            'Site:'
            'Name:'
            'Local name:'
            'Order / SubOrder:'
            'Nearest Town:'
            'Comment:'
            'GPS:'
            'GPS DATUM:'
            'MapId:'
            'Natural Vegetation:')
          Lookup = False
          LookupCaseSensitive = False
          LookupHistory = False
          BackGround.Top = 0
          BackGround.Left = 0
          BackGround.Display = bdTile
          BackGround.Cells = bcNormal
          Filter = <>
          ColWidths = (
            146
            429)
        end
      end
      object WaterSheet: TTabSheet
        Caption = 'Water Measured'
        object WaterGrid: TAdvStringGrid
          Left = 0
          Top = 38
          Width = 576
          Height = 463
          Cursor = crDefault
          Align = alClient
          BorderStyle = bsNone
          ColCount = 15
          Ctl3D = False
          DefaultColWidth = 55
          DefaultRowHeight = 21
          DefaultDrawing = False
          FixedCols = 0
          RowCount = 15
          FixedRows = 1
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Tahoma'
          Font.Style = []
          GridLineWidth = 1
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goDrawFocusSelected, goColSizing]
          ParentCtl3D = False
          ParentFont = False
          TabOrder = 0
          OnKeyDown = GridKeyDown
          OnMouseUp = GridMouseUp
          OnSetEditText = GridSetEditText
          GridLineColor = clSilver
          OnGetDisplText = GridGetDisplText
          ActiveCellShow = False
          ActiveCellFont.Charset = DEFAULT_CHARSET
          ActiveCellFont.Color = clWindowText
          ActiveCellFont.Height = -11
          ActiveCellFont.Name = 'MS Sans Serif'
          ActiveCellFont.Style = [fsBold]
          ActiveCellColor = clGray
          Bands.PrimaryColor = clInfoBk
          Bands.PrimaryLength = 1
          Bands.SecondaryColor = clWindow
          Bands.SecondaryLength = 1
          Bands.Print = False
          AutoNumAlign = False
          AutoSize = False
          VAlignment = vtaTop
          EnhTextSize = False
          EnhRowColMove = False
          SizeWithForm = False
          Multilinecells = True
          OnGetCellColor = WaterGridGetCellColor
          OnCanEditCell = GridCanEditCell
          DragDropSettings.OleAcceptFiles = True
          DragDropSettings.OleAcceptText = True
          SortSettings.AutoColumnMerge = False
          SortSettings.Column = 0
          SortSettings.Show = False
          SortSettings.IndexShow = False
          SortSettings.IndexColor = clYellow
          SortSettings.Full = True
          SortSettings.SingleColumn = False
          SortSettings.IgnoreBlanks = False
          SortSettings.BlankPos = blFirst
          SortSettings.AutoFormat = True
          SortSettings.Direction = sdAscending
          SortSettings.FixedCols = False
          SortSettings.NormalCellsOnly = False
          SortSettings.Row = 0
          FloatingFooter.Color = clMoneyGreen
          FloatingFooter.Column = 0
          FloatingFooter.FooterStyle = fsFixedLastRow
          FloatingFooter.Visible = True
          ControlLook.Color = clBlack
          ControlLook.CheckSize = 15
          ControlLook.RadioSize = 10
          ControlLook.ControlStyle = csClassic
          ControlLook.FlatButton = False
          EnableBlink = False
          EnableHTML = True
          EnableWheel = True
          Flat = True
          HintColor = clInfoBk
          SelectionColor = clHighlight
          SelectionTextColor = clHighlightText
          SelectionRectangle = False
          SelectionResizer = False
          SelectionRTFKeep = False
          HintShowCells = False
          HintShowLargeText = False
          HintShowSizing = False
          PrintSettings.FooterSize = 0
          PrintSettings.HeaderSize = 0
          PrintSettings.Time = ppNone
          PrintSettings.Date = ppNone
          PrintSettings.DateFormat = 'dd/mm/yyyy'
          PrintSettings.PageNr = ppNone
          PrintSettings.Title = ppNone
          PrintSettings.Font.Charset = DEFAULT_CHARSET
          PrintSettings.Font.Color = clWindowText
          PrintSettings.Font.Height = -11
          PrintSettings.Font.Name = 'MS Sans Serif'
          PrintSettings.Font.Style = []
          PrintSettings.HeaderFont.Charset = DEFAULT_CHARSET
          PrintSettings.HeaderFont.Color = clWindowText
          PrintSettings.HeaderFont.Height = -11
          PrintSettings.HeaderFont.Name = 'MS Sans Serif'
          PrintSettings.HeaderFont.Style = []
          PrintSettings.FooterFont.Charset = DEFAULT_CHARSET
          PrintSettings.FooterFont.Color = clWindowText
          PrintSettings.FooterFont.Height = -11
          PrintSettings.FooterFont.Name = 'MS Sans Serif'
          PrintSettings.FooterFont.Style = []
          PrintSettings.Borders = pbNoborder
          PrintSettings.BorderStyle = psSolid
          PrintSettings.Centered = False
          PrintSettings.RepeatFixedRows = False
          PrintSettings.RepeatFixedCols = False
          PrintSettings.LeftSize = 0
          PrintSettings.RightSize = 0
          PrintSettings.ColumnSpacing = 0
          PrintSettings.RowSpacing = 0
          PrintSettings.TitleSpacing = 0
          PrintSettings.Orientation = poPortrait
          PrintSettings.PageNumberOffset = 0
          PrintSettings.MaxPagesOffset = 0
          PrintSettings.FixedWidth = 0
          PrintSettings.FixedHeight = 0
          PrintSettings.UseFixedHeight = False
          PrintSettings.UseFixedWidth = False
          PrintSettings.FitToPage = fpNever
          PrintSettings.PageNumSep = '/'
          PrintSettings.NoAutoSize = False
          PrintSettings.NoAutoSizeRow = False
          PrintSettings.PrintGraphics = False
          HTMLSettings.Width = 100
          HTMLSettings.XHTML = False
          Navigation.AdvanceOnEnter = True
          Navigation.AdvanceDirection = adTopBottom
          Navigation.AllowClipboardShortCuts = True
          Navigation.AllowClipboardAlways = True
          Navigation.InsertPosition = pInsertBefore
          Navigation.HomeEndKey = heFirstLastColumn
          Navigation.TabToNextAtEnd = False
          Navigation.TabAdvanceDirection = adLeftRight
          ColumnSize.Location = clRegistry
          CellNode.Color = clSilver
          CellNode.NodeColor = clBlack
          CellNode.ShowTree = False
          MaxEditLength = 0
          MouseActions.DisjunctCellSelect = True
          MouseActions.RangeSelectAndEdit = True
          IntelliPan = ipVertical
          URLColor = clBlue
          URLShow = False
          URLFull = False
          URLEdit = False
          ScrollType = ssNormal
          ScrollColor = clNone
          ScrollWidth = 17
          ScrollSynch = False
          ScrollProportional = False
          ScrollHints = shNone
          OemConvert = False
          FixedFooters = 1
          FixedRightCols = 0
          FixedColWidth = 55
          FixedRowHeight = 21
          FixedFont.Charset = ANSI_CHARSET
          FixedFont.Color = clWindowText
          FixedFont.Height = -13
          FixedFont.Name = 'Arial'
          FixedFont.Style = []
          FixedAsButtons = False
          FloatFormat = '%.2f'
          IntegralHeight = False
          WordWrap = False
          ColumnHeaders.Strings = (
            'Depth\n(mm)'
            'BD\n(g/cc)'
            'SAT\n(mm/mm)'
            'DUL\n(mm/mm)'
            'LL15\n(mm/mm)'
            'PAWC\n(mm)'
            'Airdry\n(mm/mm)')
          Lookup = False
          LookupCaseSensitive = False
          LookupHistory = False
          BackGround.Top = 0
          BackGround.Left = 0
          BackGround.Display = bdTile
          BackGround.Cells = bcNormal
          Filter = <>
          ColWidths = (
            55
            55
            55
            55
            55
            55
            55
            55
            55
            55
            55
            55
            55
            55
            55)
          RowHeights = (
            46
            21
            21
            21
            21
            21
            21
            21
            21
            21
            21
            21
            21
            21
            21)
        end
        object Panel1: TPanel
          Left = 0
          Top = 0
          Width = 576
          Height = 38
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 1
          object LockButton: TSpeedButton
            Left = 8
            Top = 0
            Width = 120
            Height = 38
            AllowAllUp = True
            GroupIndex = 1
            Caption = 'Lock columns'
            Flat = True
            Glyph.Data = {
              76060000424D7606000000000000360400002800000018000000180000000100
              0800000000004002000000000000000000000001000000000000000000000000
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
              FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FCFCFCFCFCFC
              FCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFC
              FC6565FCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFC652F2625FCFCFCFC
              FCFCFCFCFCFCFCFCFCFCFCFCFCFCFCAD2E37371DADFCFCFCFCFCFCFCFCFCFCFC
              FCFCFCFCFCAE261D1C37372E1DFCFCFCFCFCFCFCFCFCFCFCFCFCFCFC6E2F3725
              37373737251D1DAEFCFCFCFCFCFCFCFCFCFCFCFC6E2F37373F373F7F2F2E261D
              FCFCFCFCFCFCFCFCFCFCFCFCFC253F3F3F7F7F7F372F372665FCFCFCFCFCFCFC
              FCFCFCFCFCAE2F7F7F7F7F2F373FBFBF65FCFCFCFCAEAEFCFCFCFCFCFCFC667F
              7F37373FBFBF6E65FCFCFCFC2E2F372F6EFCFCFCFC6E263737377FBFB765FCFC
              FCFCFC6E2F2F2F373F6EFCFC262E37377FBFBF26AEFCFCFCFCFCFC2E3F3F3F2F
              373F26262F377FBFBF6E6EFCFCFCFCFCFCFCFC2E7F7FBF7F2F3737373FBFBF77
              6EFCFCFCFCFCFCFCFCFCFC2E7F7FAEFC7F2F377FBF7F6EFCFCFCFCFCFCFCFCFC
              FCFCFC6E7F77B6FCFC7FBFBF2EAEFCFCFCFCFCFCFCFCFCFCFCFCFCAE777777FC
              FC7F2F3F2FFCFCFCFCFCFCFCFCFCFCFCFCFCFCFC377F77B6FCB67F7F2FFCFCFC
              FCFCFCFCFCFCFCFCFCFCFCFC777F7777FCFC7F7F37FCFCFCFCFCFCFCFCFCFCFC
              FCFCFCFCFC377F7777777F7F37FCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFC37BF
              7F7F7F7F37FCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFC37BFBFBF7F77FCFCFC
              FCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFC7677376FFCFCFCFCFCFCFCFCFCFCFCFC
              FCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFC}
            OnClick = LockLabelClick
          end
          object GraphButton: TSpeedButton
            Left = 128
            Top = 0
            Width = 90
            Height = 38
            Hint = 'Graph of soil water.'
            Caption = 'Graph'
            Flat = True
            Glyph.Data = {
              36090000424D3609000000000000360000002800000018000000180000000100
              20000000000000090000120B0000120B00000000000000000000FFFFFF00FFFF
              FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
              FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
              FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00BEA7
              A7A0B29494D7B09494CBB29595BEB39898B2B99F9FA5BFA8A898C4AEAE8CC9B6
              B67FCFBCBC73D4C3C366DACACA5ADFD1D14DE4DADA40E9E0E034EEE7E727F5F0
              F01AF9F7F70EFDFCFC05FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00B49C
              9CB2FBEAEAFFF6E6E6FFF6E6E6FFF4E3E3FFF1E0E0FFCCBBBBFFE8D3D3FFE0CA
              CAFFDBC5C5FFD9BFBFFFBDA4A4FFCEB0B0FFC6A8A8FFC1A1A1FFBE9C9CFFAB89
              89FFB58E8EFEB18B8BFBBB8F8FF3BA8E8EE7D6BDBD74FFFFFF00FFFFFF00B49C
              9CB0F4E7E8FFD5CACFFFD9CCCFFFF1E0E0FFF1DFDFFFD0C0C0FFF1E1E1FFEFDE
              DEFFF0DDDDFFF3E0E0FFD0C1C1FFF2DEDEFFEEDCDCFFEFDBDBFFF1DDDDFFD0BE
              BEFFF1DCDCFFEFD9D9FFEFDADAFFE3C9C9FFC7ACAC86FFFFFF00FFFFFF00B6A1
              A3ADDDBDABFFB85A0CFF9B8177FFDDD4DAFFF6E6E7FFD1C2C2FFF1E0E0FFEEDE
              DEFFEEDCDCFFF0DEDEFFCFBEBEFFF0DDDDFFECD9D9FFECD8D8FFEEDADAFFCDBB
              BBFFEDD6D6FFE9D3D3FFEBD4D4FFDFC6C6FFC6ACAC86FFFFFF00FFFFFF00B7A4
              A6A9EDC3A6FFDD8021FFC75F01FFB8713DFFD3C5C6FFCFC7CAFFF3E3E5FFF3E3
              E4FFF1E0E0FFF3E2E2FFD1C2C2FFF1DFDFFFEFDBDBFFEDDADAFFF0DBDBFFCFBE
              BEFFEFDADAFFEBD5D5FFEBD6D6FFE1C7C7FFC6AEAE86FFFFFF00FFFFFF00BAA6
              A6A6D3C9CCFFD0C9CFFFDBBEA6FFE59746FFC15500FF9D6640FF968680FFBFB5
              B6FFDCCDCDFFDDCECEFFC0B3B3FFDECECEFFDECDCDFFE0CECEFFE1CFCFFFC4B4
              B4FFE3CFCFFFE1CDCDFFE3CFCFFFDAC1C1FFC6ADAD86FFFFFF00FFFFFF00B8A6
              A6A4F6EAEAFFF4E8E8FFF3E8EBFFF3EAEDFFF8D1AFFFD67515FFBC5000FFB0AC
              B3FFF1E2E2FFEDDDDDFFCDBFBFFFEBDADAFFE7D6D6FFE6D4D4FFE8D5D5FFC8B8
              B8FFE5D2D2FFE2CECEFFE1CECEFFD6C0C0FFC6AFAF86FFFFFF00FFFFFF00BBAA
              AAA0F4E7E7FFF2E7E7FFF2E5E5FFF2E6E6FFF5EDF4FFCFA989FFD9791CFFC4A2
              8FFFF6EBEDFFF3E4E4FFD4C6C6FFF2E2E2FFF0DFDFFFEFDEDEFFF2DFDFFFD1C2
              C2FFEFDCDCFFEED9D9FFEEDADAFFE2CACAFFC6AEAE86FFFFFF00FFFFFF00BBAC
              AC9DF2E6E6FFF4E8E8FFF3E7E7FFF3E7E7FFF6E9E9FFD6D1D8FFF3B679FFC26E
              25FFE7E0E8FFF5E7E8FFD5C7C7FFF3E4E4FFEFDFDFFFF0DFDFFFF4E3E3FFD5C5
              C5FFF1DEDEFFECD9D9FFEEDBDBFFE2CBCBFFC6B0B086FFFFFF00FFFFFF00BDAF
              AF9AF4E8E8FFF7ECECFFF6EBEBFFF7ECECFFF7ECECFFD8D0D3FFF3D5BFFFDD83
              28FFC8ADA1FFFAEEF0FFD8CCCCFFF7E8E8FFF3E4E4FFF3E4E6FFCCBEC3FFA19A
              A0FFE8DCE0FFF4E3E6FFECDCDDFFE1CACBFFC7B0B086FFFFFF00FFFFFF00C0B3
              B397DFD7D7FFE3DADAFFE3DADAFFE2DADAFFE3DADAFFC7BEBFFFE3DEE5FFEBAB
              69FFBB6A28FFDED7E0FFB9A5AEFFCCBABFFFE2D5D7FFDACBCAFFCB6403FFA848
              01FFAD6A39FFBB8C71FFA57C66FFA09191FFC3AEAE86FFFFFF00FFFFFF00C0B6
              B693E9DEE1FFE1D2D9FFDED2D5FFEDE4E4FFEEE4E4FFCFC7C7FFEDE4E7FFEFCF
              B7FFDC751BFFA69F89FF13AE30FF4E8F4AFFDAC3D5FFD28C58FFDA7C1BFFCD8B
              52FFE49343FFCF6803FFDC7914FFA36A46FFBCABAC8AFFFFFF00FFFFFF00C3B8
              BA90D2D7CCFF37A840FF99A693FFF2E2E8FFFEF4F4FFDBD3D3FFF9EFF0FFF9F0
              F6FFF0A863FF2FB749FF36DC6FFF02B222FF3AA640FFCE6200FFEDB999FFD9D2
              DCFFF5EAF2FFF0E0E0FFDDA880FFD3BEBDFFC7B0B08AFFFFFF00FFFFFF00C5BB
              BC8EAED8B5FF23D861FF009D08FF88B483FFFCE7F4FFE1D6D9FFFDF2F3FFFFF0
              F9FF4DD07AFF1DB941FFB8A8A2FFC4DABDFF20BD47FF11B32FFF79A57DFF9F91
              93FFE7D6D7FFF1E1E2FFF1E2E5FFE6D3D4FFC5AFAF8AFFFFFF00FFFFFF00C5BE
              BE8AFAEAF0FFE7EEE5FF64CC78FF00A508FF33A634FFBCB9B6FFFFE5F4FF6CD6
              8DFF12C249FFCAA14BFFAD663DFFCD6E23FFDC832DFF6ADA93FF14C440FF229E
              2EFFD1B5C0FFF6E4E4FFF1E1E1FFE7D3D3FFC5AEAE8AFFFFFF00FFFFFF00C6BF
              BF87E6DFDFFFF4ECEFFFFFEDF7FFB6D8BDFF12B12FFF009F08FF37A042FF0DBC
              3AFF9EC6A3FFF5C8ACFFBE5000FFC35C00FFCBB3AAFFF4DAE7FF72CE8EFF0EBD
              39FF71AE75FFE2C8D0FFDECECFFFD5C3C3FFC5AFAF8AFFFFFF00FFFFFF00C7C1
              C184DAD5D5FFE7E2E2FFE8E2E3FFEFE4E9FFF6EAEFFF53BF6AFF24D55DFF4BA2
              4FFFEDDAE5FFECDCD9FFDF872BFFB37849FFE2DDE2FFF0E4E4FFFFE3EDFF7CCC
              94FF07B930FF93B28CFFF7DFE5FFE9D6D6FFC5B1B18AFFFFFF00FFFFFF00C8C3
              C381EFEBEBFFFFFBFBFFFCF8F8FFFDF7F7FFFFF8FAFFE8DBE1FFB5E1BDFFEBE2
              E3FFFDF4F5FFFAF2F4FFDCD7DDFFF8F2F7FFF7EBEBFFF4E8E8FFF6E8E8FFE7CD
              D6FF68D388FF07B027FF959E8BFFD3BCC1FFC7B1B28AFFFFFF00FFFFFF00CAC7
              C77DEDE9E9FFFDFAFAFFFBF8F8FFFAF7F7FFFDF8F8FFE0DADAFFFFF6F8FFFBF3
              F4FFF7EFEFFFF8F1F1FFDCD3D3FFF8EDEDFFF4E9E9FFF3E7E7FFF4E7E7FFDACD
              CEFFFFE7F0FF3BC159FF05A81BFF909683FFC1AEAE8AFFFFFF00FFFFFF00C9C8
              C87BECEAEAFFFEFDFDFFFCF9F9FFFBF8F8FFFDF9F9FFE0DBDBFFFCF6F6FFF9F2
              F2FFF9F2F2FFFAF2F2FFDDD6D6FFF9F0F0FFF7ECECFFF6EAEAFFF7ECECFFDBCF
              CFFFFFE9EEFFACDCB4FF28D25CFFA2AA95FFC7B1B28AFFFFFF00FFFFFF00CDCA
              CA78F2F0F0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE2DEDEFFFAF5F5FFF3ED
              EDFFF0E7E7FFEDE3E3FFD1C9C9FFE5D7D7FFDED1D1FFDACBCBFFD8C7C7FFC3B0
              B0FFD1BBBCFFD3B6BAFFD4B0B8FFC8A7AAFFC6B1B18BFFFFFF00FFFFFF00D9D8
              D860B1AEAECCB1ADADC4B0ACACBCB1ACACB4B4AEAEACB9B2B2A4BCB3B39CC0B6
              B694C4BABA8CC7BEBE84CAC0C07CCDC4C474D0C7C76CD5CACA64D8CECE5CDCD3
              D354DFD5D54CE3DADA44E6DEDE3CEAE2E234F4F1F11AFFFFFF00FFFFFF00FFFF
              FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
              FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
              FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00}
            ParentShowHint = False
            ShowHint = True
            OnClick = GraphButtonClick
          end
          object NewCropButton: TSpeedButton
            Left = 216
            Top = 0
            Width = 113
            Height = 38
            Hint = 'Add a new crop to this soil.'
            Caption = 'New Crop'
            Flat = True
            Glyph.Data = {
              F6060000424DF606000000000000360000002800000017000000180000000100
              180000000000C0060000130B0000130B00000000000000000000D8DDE0D8DDE0
              D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE00080000080
              00D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE000
              0000D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0
              D8DDE0008000008000D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DD
              E0D8DDE0D8DDE0000000D8DDE0D8DDE0D8DDE0D8DDE001010101010101010101
              0101010101010101010101008000008000010101010101010101010101010101
              010101010101D8DDE0D8DDE0D8DDE0000000D8DDE0D8DDE0D8DDE0D8DDE00101
              01FEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFE008000008000FEFEFEFEFEFEFE
              FEFEFEFEFEFEFEFEFEFEFE010101D8DDE0D8DDE0D8DDE0000000D8DDE0D8DDE0
              D8DDE0D8DDE0010101FEFEFEFEFEFEFEFEFE008000FEFEFEFEFEFE0080000080
              00FEFEFEFEFEFE008000FEFEFEFEFEFEFEFEFE010101D8DDE0D8DDE0D8DDE000
              0000D8DDE0D8DDE0D8DDE0D8DDE0010101FEFEFEFEFEFEFEFEFE008000008000
              FEFEFE008000008000FEFEFE008000008000FEFEFEFEFEFEFEFEFE010101D8DD
              E0D8DDE0D8DDE0000000D8DDE0D8DDE0D8DDE0D8DDE0010101FEFEFEFEFEFEFE
              FEFEFEFEFE008000008000008000008000008000008000FEFEFEFEFEFEFEFEFE
              FEFEFE010101D8DDE0D8DDE0D8DDE0000000D8DDE0D8DDE0D8DDE0D8DDE00101
              01FEFEFE008000008000FEFEFEFEFEFEFEFEFE008000008000FEFEFEFEFEFEFE
              FEFE008000FEFEFEFEFEFE010101D8DDE0D8DDE0D8DDE0000000D8DDE0D8DDE0
              D8DDE0D8DDE0010101FEFEFEFEFEFE008000FEFEFEFEFEFE0080000080000080
              00FEFEFEFEFEFE008000008000FEFEFEFEFEFE010101D8DDE0D8DDE0D8DDE000
              0000D8DDE0D8DDE0D8DDE0D8DDE0010101FEFEFEFEFEFE008000008000008000
              008000008000008000008000008000008000008000FEFEFEFEFEFE010101D8DD
              E0D8DDE0D8DDE0000000D8DDE0D8DDE0D8DDE0D8DDE0010101FEFEFEFEFEFEFE
              FEFEFEFEFEFEFEFEFEFEFE008000008000FEFEFEFEFEFEFEFEFEFEFEFEFEFEFE
              FEFEFE010101D8DDE0D8DDE0D8DDE0000000D8DDE0D8DDE0D8DDE0D8DDE00101
              01FEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFE008000008000FEFEFEFEFEFE00
              8000FEFEFEFEFEFEFEFEFE010101D8DDE0D8DDE0D8DDE0000000D8DDE0D8DDE0
              D8DDE0D8DDE0010101FEFEFEFEFEFEFEFEFE0080000080000080000080000080
              00008000FEFEFE008000FEFEFEFEFEFEFEFEFE010101D8DDE0D8DDE0D8DDE000
              0000D8DDE0D8DDE0D8DDE0D8DDE0010101FEFEFEFEFEFEFEFEFE008000008000
              FEFEFE00FFFF00FFFF008000008000008000FFFFFFFFFFFFFFFFFF010101D8DD
              E0D8DDE0D8DDE0000000D8DDE0D8DDE0D8DDE0D8DDE0010101FEFEFEFEFEFEFE
              FEFEFEFEFEFEFEFEFEFEFE00808000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFF010101D8DDE0D8DDE0D8DDE0000000D8DDE0D8DDE0D8DDE0D8DDE00101
              01FEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFE00FFFF008080FEFEFE00000000
              0000000000000000000000010101D8DDE0D8DDE0D8DDE0000000D8DDE0D8DDE0
              D8DDE0D8DDE0010101FEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFE
              FEFEFEFE000000FEFEFEFFFFFFFFFFFFFFFFFF000000D8DDE0D8DDE0D8DDE000
              0000D8DDE0D8DDE0D8DDE0D8DDE0010101FEFEFEFEFEFEFEFEFEFEFEFEFEFEFE
              FEFEFEFEFEFEFEFEFEFEFEFE000000FFFFFFFFFFFFFFFFFF000000D8DDE0D8DD
              E0D8DDE0D8DDE0000000D8DDE0D8DDE0D8DDE0D8DDE0010101FEFEFEFEFEFEFE
              FEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFE000000FFFFFF000000000000
              D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0000000D8DDE0D8DDE0D8DDE0D8DDE00101
              0101010101010101010101010101010101010101010101010101010101010100
              0000D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0000000D8DDE0D8DDE0
              D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DD
              E0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE000
              0000D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0
              D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DD
              E0D8DDE0D8DDE0000000D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8
              DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0
              D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0000000D8DDE0D8DDE0D8DDE0D8DDE0D8DD
              E0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8
              DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0000000}
            ParentShowHint = False
            ShowHint = True
            OnClick = NewCropButtonClick
          end
          object DeleteCropButton: TSpeedButton
            Left = 328
            Top = 0
            Width = 113
            Height = 38
            Hint = 'Delete a crop form this soil.'
            Caption = 'Delete Crop'
            Flat = True
            Glyph.Data = {
              F6060000424DF606000000000000360000002800000017000000180000000100
              180000000000C0060000130B0000130B00000000000000000000D8DDE0D8DDE0
              D8DDE0D8DDE00000FF0000FFD8DDE0D8DDE0D8DDE0D8DDE0D8DDE00080000080
              00D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE00000FFD8DDE0D8DDE000
              0000D8DDE0D8DDE0D8DDE00000FF0000FF0000FF0000FFD8DDE0D8DDE0D8DDE0
              D8DDE0008000008000D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DD
              E0D8DDE0D8DDE0000000D8DDE0D8DDE0D8DDE00000FF0000FF0000FF0000FF00
              00FF010101010101010101008000008000010101010101010101010101010101
              0000FF0000FFD8DDE0D8DDE0D8DDE0000000D8DDE0D8DDE0D8DDE00000FF0000
              FF0000FF0000FF0000FFFEFEFEFEFEFEFEFEFE008000008000FEFEFEFEFEFEFE
              FEFEFEFEFE0000FF0000FF010101D8DDE0D8DDE0D8DDE0000000D8DDE0D8DDE0
              D8DDE00000FF0000FF0000FF0000FF0000FF008000FEFEFEFEFEFE0080000080
              00FEFEFEFEFEFE008000FEFEFE0000FF0000FF010101D8DDE0D8DDE0D8DDE000
              0000D8DDE0D8DDE0D8DDE0D8DDE00000FF0000FF0000FF0000FF0000FF008000
              FEFEFE008000008000FEFEFE008000008000FEFEFE0000FF0000FF010101D8DD
              E0D8DDE0D8DDE0000000D8DDE0D8DDE0D8DDE0D8DDE0010101FEFEFE0000FF00
              00FF0000FF0000FF008000008000008000008000008000FEFEFE0000FF0000FF
              FEFEFE010101D8DDE0D8DDE0D8DDE0000000D8DDE0D8DDE0D8DDE0D8DDE00101
              01FEFEFE0080000000FF0000FF0000FF0000FF008000008000FEFEFE0000FF00
              00FF0000FFFEFEFEFEFEFE010101D8DDE0D8DDE0D8DDE0000000D8DDE0D8DDE0
              D8DDE0D8DDE0010101FEFEFEFEFEFE0080000000FF0000FF0000FF0000FF0000
              FF0000FF0000FF0000FF008000FEFEFEFEFEFE010101D8DDE0D8DDE0D8DDE000
              0000D8DDE0D8DDE0D8DDE0D8DDE0010101FEFEFEFEFEFE008000008000008000
              0000FF0000FF0000FF0000FF008000008000008000FEFEFEFEFEFE010101D8DD
              E0D8DDE0D8DDE0000000D8DDE0D8DDE0D8DDE0D8DDE0010101FEFEFEFEFEFEFE
              FEFEFEFEFEFEFEFE0000FF0000FF0000FF0000FFFEFEFEFEFEFEFEFEFEFEFEFE
              FEFEFE010101D8DDE0D8DDE0D8DDE0000000D8DDE0D8DDE0D8DDE0D8DDE00101
              01FEFEFEFEFEFEFEFEFEFEFEFE0000FF0000FF0000FF0000FF0000FF0000FF00
              8000FEFEFEFEFEFEFEFEFE010101D8DDE0D8DDE0D8DDE0000000D8DDE0D8DDE0
              D8DDE0D8DDE0010101FEFEFEFEFEFEFEFEFE0000FF0000FF0000FF0000FF0080
              000000FF0000FF0000FFFEFEFEFEFEFEFEFEFE010101D8DDE0D8DDE0D8DDE000
              0000D8DDE0D8DDE0D8DDE0D8DDE0010101FEFEFEFEFEFE0000FF0000FF0000FF
              0000FF00FFFF00FFFF0080000000FF0000FF0000FFFFFFFFFFFFFF010101D8DD
              E0D8DDE0D8DDE0000000D8DDE0D8DDE0D8DDE0D8DDE00000FF0000FF0000FF00
              00FF0000FF0000FFFEFEFE00808000FFFFFFFFFFFFFFFFFFFFFF0000FF0000FF
              FFFFFF010101D8DDE0D8DDE0D8DDE0000000D8DDE0D8DDE00000FF0000FF0000
              FF0000FF0000FF0000FFFEFEFEFEFEFEFEFEFE00FFFF008080FEFEFE00000000
              00000000000000FF0000FF010101D8DDE0D8DDE0D8DDE0000000D8DDE0D8DDE0
              0000FF0000FF0000FF0000FF0000FFFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFE
              FEFEFEFE000000FEFEFEFFFFFFFFFFFFFFFFFF0000FF0000FFD8DDE0D8DDE000
              0000D8DDE0D8DDE00000FF0000FF0000FF0000FF0000FFFEFEFEFEFEFEFEFEFE
              FEFEFEFEFEFEFEFEFEFEFEFE000000FFFFFFFFFFFFFFFFFF000000D8DDE00000
              FFD8DDE0D8DDE0000000D8DDE0D8DDE0D8DDE00000FF0000FFFEFEFEFEFEFEFE
              FEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFE000000FFFFFF000000000000
              D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0000000D8DDE0D8DDE0D8DDE0D8DDE00101
              0101010101010101010101010101010101010101010101010101010101010100
              0000D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0000000D8DDE0D8DDE0
              D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DD
              E0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE000
              0000D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0
              D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DD
              E0D8DDE0D8DDE0000000D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8
              DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0
              D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0000000D8DDE0D8DDE0D8DDE0D8DDE0D8DD
              E0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8
              DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0D8DDE0000000}
            ParentShowHint = False
            ShowHint = True
            OnClick = DeleteCropButtonClick
          end
        end
      end
      object WaterPredictedSheet: TTabSheet
        Caption = 'Water Predicted'
        ImageIndex = 5
        DesignSize = (
          576
          501)
        object WaterPredictedGrid: TAdvStringGrid
          Left = 0
          Top = 0
          Width = 576
          Height = 380
          Cursor = crDefault
          Anchors = [akLeft, akTop, akRight]
          BorderStyle = bsNone
          ColCount = 15
          Ctl3D = False
          DefaultColWidth = 55
          DefaultRowHeight = 21
          DefaultDrawing = False
          FixedCols = 0
          RowCount = 15
          FixedRows = 1
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Arial'
          Font.Style = []
          GridLineWidth = 1
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goDrawFocusSelected, goColSizing]
          ParentCtl3D = False
          ParentFont = False
          TabOrder = 0
          OnKeyDown = GridKeyDown
          OnMouseUp = GridMouseUp
          OnSetEditText = GridSetEditText
          GridLineColor = clSilver
          OnGetDisplText = GridGetDisplText
          ActiveCellShow = False
          ActiveCellFont.Charset = DEFAULT_CHARSET
          ActiveCellFont.Color = clWindowText
          ActiveCellFont.Height = -11
          ActiveCellFont.Name = 'MS Sans Serif'
          ActiveCellFont.Style = [fsBold]
          ActiveCellColor = clGray
          Bands.PrimaryColor = clInfoBk
          Bands.PrimaryLength = 1
          Bands.SecondaryColor = clWindow
          Bands.SecondaryLength = 1
          Bands.Print = False
          AutoNumAlign = False
          AutoSize = False
          VAlignment = vtaTop
          EnhTextSize = False
          EnhRowColMove = False
          SizeWithForm = False
          Multilinecells = True
          OnGetCellColor = WaterPredictedGridGetCellColor
          OnCanEditCell = GridCanEditCell
          DragDropSettings.OleAcceptFiles = True
          DragDropSettings.OleAcceptText = True
          SortSettings.AutoColumnMerge = False
          SortSettings.Column = 0
          SortSettings.Show = False
          SortSettings.IndexShow = False
          SortSettings.IndexColor = clYellow
          SortSettings.Full = True
          SortSettings.SingleColumn = False
          SortSettings.IgnoreBlanks = False
          SortSettings.BlankPos = blFirst
          SortSettings.AutoFormat = True
          SortSettings.Direction = sdAscending
          SortSettings.FixedCols = False
          SortSettings.NormalCellsOnly = False
          SortSettings.Row = 0
          FloatingFooter.Color = clMoneyGreen
          FloatingFooter.Column = 0
          FloatingFooter.FooterStyle = fsFixedLastRow
          FloatingFooter.Visible = True
          ControlLook.Color = clBlack
          ControlLook.CheckSize = 15
          ControlLook.RadioSize = 10
          ControlLook.ControlStyle = csClassic
          ControlLook.FlatButton = False
          EnableBlink = False
          EnableHTML = True
          EnableWheel = True
          Flat = True
          HintColor = clInfoBk
          SelectionColor = clHighlight
          SelectionTextColor = clHighlightText
          SelectionRectangle = False
          SelectionResizer = False
          SelectionRTFKeep = False
          HintShowCells = False
          HintShowLargeText = False
          HintShowSizing = False
          PrintSettings.FooterSize = 0
          PrintSettings.HeaderSize = 0
          PrintSettings.Time = ppNone
          PrintSettings.Date = ppNone
          PrintSettings.DateFormat = 'dd/mm/yyyy'
          PrintSettings.PageNr = ppNone
          PrintSettings.Title = ppNone
          PrintSettings.Font.Charset = DEFAULT_CHARSET
          PrintSettings.Font.Color = clWindowText
          PrintSettings.Font.Height = -11
          PrintSettings.Font.Name = 'MS Sans Serif'
          PrintSettings.Font.Style = []
          PrintSettings.HeaderFont.Charset = DEFAULT_CHARSET
          PrintSettings.HeaderFont.Color = clWindowText
          PrintSettings.HeaderFont.Height = -11
          PrintSettings.HeaderFont.Name = 'MS Sans Serif'
          PrintSettings.HeaderFont.Style = []
          PrintSettings.FooterFont.Charset = DEFAULT_CHARSET
          PrintSettings.FooterFont.Color = clWindowText
          PrintSettings.FooterFont.Height = -11
          PrintSettings.FooterFont.Name = 'MS Sans Serif'
          PrintSettings.FooterFont.Style = []
          PrintSettings.Borders = pbNoborder
          PrintSettings.BorderStyle = psSolid
          PrintSettings.Centered = False
          PrintSettings.RepeatFixedRows = False
          PrintSettings.RepeatFixedCols = False
          PrintSettings.LeftSize = 0
          PrintSettings.RightSize = 0
          PrintSettings.ColumnSpacing = 0
          PrintSettings.RowSpacing = 0
          PrintSettings.TitleSpacing = 0
          PrintSettings.Orientation = poPortrait
          PrintSettings.PageNumberOffset = 0
          PrintSettings.MaxPagesOffset = 0
          PrintSettings.FixedWidth = 0
          PrintSettings.FixedHeight = 0
          PrintSettings.UseFixedHeight = False
          PrintSettings.UseFixedWidth = False
          PrintSettings.FitToPage = fpNever
          PrintSettings.PageNumSep = '/'
          PrintSettings.NoAutoSize = False
          PrintSettings.NoAutoSizeRow = False
          PrintSettings.PrintGraphics = False
          HTMLSettings.Width = 100
          HTMLSettings.XHTML = False
          Navigation.AdvanceOnEnter = True
          Navigation.AdvanceDirection = adTopBottom
          Navigation.AllowClipboardShortCuts = True
          Navigation.AllowSmartClipboard = True
          Navigation.AllowClipboardAlways = True
          Navigation.InsertPosition = pInsertBefore
          Navigation.HomeEndKey = heFirstLastColumn
          Navigation.TabToNextAtEnd = False
          Navigation.TabAdvanceDirection = adLeftRight
          ColumnSize.Location = clRegistry
          CellNode.Color = clSilver
          CellNode.NodeColor = clBlack
          CellNode.ShowTree = False
          MaxEditLength = 0
          MouseActions.DisjunctCellSelect = True
          MouseActions.RangeSelectAndEdit = True
          IntelliPan = ipVertical
          URLColor = clBlue
          URLShow = False
          URLFull = False
          URLEdit = False
          ScrollType = ssNormal
          ScrollColor = clNone
          ScrollWidth = 17
          ScrollSynch = False
          ScrollProportional = False
          ScrollHints = shNone
          OemConvert = False
          FixedFooters = 1
          FixedRightCols = 0
          FixedColWidth = 55
          FixedRowHeight = 21
          FixedFont.Charset = ANSI_CHARSET
          FixedFont.Color = clWindowText
          FixedFont.Height = -13
          FixedFont.Name = 'Arial'
          FixedFont.Style = []
          FixedAsButtons = False
          FloatFormat = '%.2f'
          IntegralHeight = False
          WordWrap = False
          ColumnHeaders.Strings = (
            'Depth\n(cm)')
          Lookup = False
          LookupCaseSensitive = False
          LookupHistory = False
          BackGround.Top = 0
          BackGround.Left = 0
          BackGround.Display = bdTile
          BackGround.Cells = bcNormal
          Filter = <>
          RowHeights = (
            41
            21
            21
            21
            21
            21
            21
            21
            21
            21
            21
            21
            21
            21
            21)
        end
      end
      object ProfileSheet: TTabSheet
        Caption = 'Profile'
        ImageIndex = 1
        object ProfileGrid: TAdvStringGrid
          Left = 0
          Top = 0
          Width = 576
          Height = 501
          Cursor = crDefault
          Align = alClient
          BorderStyle = bsNone
          ColCount = 19
          Ctl3D = False
          DefaultColWidth = 35
          DefaultRowHeight = 21
          DefaultDrawing = False
          FixedCols = 0
          RowCount = 15
          FixedRows = 1
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Arial'
          Font.Style = []
          GridLineWidth = 1
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goDrawFocusSelected, goColSizing]
          ParentCtl3D = False
          ParentFont = False
          TabOrder = 0
          OnKeyDown = GridKeyDown
          OnMouseUp = GridMouseUp
          OnSetEditText = GridSetEditText
          GridLineColor = clSilver
          ActiveCellShow = False
          ActiveCellFont.Charset = DEFAULT_CHARSET
          ActiveCellFont.Color = clWindowText
          ActiveCellFont.Height = -11
          ActiveCellFont.Name = 'MS Sans Serif'
          ActiveCellFont.Style = [fsBold]
          ActiveCellColor = clGray
          Bands.PrimaryColor = clInfoBk
          Bands.PrimaryLength = 1
          Bands.SecondaryColor = clWindow
          Bands.SecondaryLength = 1
          Bands.Print = False
          AutoNumAlign = False
          AutoSize = False
          VAlignment = vtaTop
          EnhTextSize = False
          EnhRowColMove = False
          SizeWithForm = False
          Multilinecells = True
          OnCanEditCell = GridProfileCanEdit
          DragDropSettings.OleAcceptFiles = True
          DragDropSettings.OleAcceptText = True
          SortSettings.AutoColumnMerge = False
          SortSettings.Column = 0
          SortSettings.Show = False
          SortSettings.IndexShow = False
          SortSettings.IndexColor = clYellow
          SortSettings.Full = True
          SortSettings.SingleColumn = False
          SortSettings.IgnoreBlanks = False
          SortSettings.BlankPos = blFirst
          SortSettings.AutoFormat = True
          SortSettings.Direction = sdAscending
          SortSettings.FixedCols = False
          SortSettings.NormalCellsOnly = False
          SortSettings.Row = 0
          FloatingFooter.Color = clBtnFace
          FloatingFooter.Column = 0
          FloatingFooter.FooterStyle = fsFixedLastRow
          FloatingFooter.Visible = False
          ControlLook.Color = clBlack
          ControlLook.CheckSize = 15
          ControlLook.RadioSize = 10
          ControlLook.ControlStyle = csClassic
          ControlLook.FlatButton = False
          EnableBlink = False
          EnableHTML = True
          EnableWheel = True
          Flat = True
          HintColor = clInfoBk
          SelectionColor = clHighlight
          SelectionTextColor = clHighlightText
          SelectionRectangle = False
          SelectionResizer = False
          SelectionRTFKeep = False
          HintShowCells = False
          HintShowLargeText = False
          HintShowSizing = False
          PrintSettings.FooterSize = 0
          PrintSettings.HeaderSize = 0
          PrintSettings.Time = ppNone
          PrintSettings.Date = ppNone
          PrintSettings.DateFormat = 'dd/mm/yyyy'
          PrintSettings.PageNr = ppNone
          PrintSettings.Title = ppNone
          PrintSettings.Font.Charset = DEFAULT_CHARSET
          PrintSettings.Font.Color = clWindowText
          PrintSettings.Font.Height = -11
          PrintSettings.Font.Name = 'MS Sans Serif'
          PrintSettings.Font.Style = []
          PrintSettings.HeaderFont.Charset = DEFAULT_CHARSET
          PrintSettings.HeaderFont.Color = clWindowText
          PrintSettings.HeaderFont.Height = -11
          PrintSettings.HeaderFont.Name = 'MS Sans Serif'
          PrintSettings.HeaderFont.Style = []
          PrintSettings.FooterFont.Charset = DEFAULT_CHARSET
          PrintSettings.FooterFont.Color = clWindowText
          PrintSettings.FooterFont.Height = -11
          PrintSettings.FooterFont.Name = 'MS Sans Serif'
          PrintSettings.FooterFont.Style = []
          PrintSettings.Borders = pbNoborder
          PrintSettings.BorderStyle = psSolid
          PrintSettings.Centered = False
          PrintSettings.RepeatFixedRows = False
          PrintSettings.RepeatFixedCols = False
          PrintSettings.LeftSize = 0
          PrintSettings.RightSize = 0
          PrintSettings.ColumnSpacing = 0
          PrintSettings.RowSpacing = 0
          PrintSettings.TitleSpacing = 0
          PrintSettings.Orientation = poPortrait
          PrintSettings.PageNumberOffset = 0
          PrintSettings.MaxPagesOffset = 0
          PrintSettings.FixedWidth = 0
          PrintSettings.FixedHeight = 0
          PrintSettings.UseFixedHeight = False
          PrintSettings.UseFixedWidth = False
          PrintSettings.FitToPage = fpNever
          PrintSettings.PageNumSep = '/'
          PrintSettings.NoAutoSize = False
          PrintSettings.NoAutoSizeRow = False
          PrintSettings.PrintGraphics = False
          HTMLSettings.Width = 100
          HTMLSettings.XHTML = False
          Navigation.AdvanceOnEnter = True
          Navigation.AdvanceDirection = adTopBottom
          Navigation.AllowClipboardShortCuts = True
          Navigation.AllowClipboardAlways = True
          Navigation.InsertPosition = pInsertBefore
          Navigation.HomeEndKey = heFirstLastColumn
          Navigation.TabToNextAtEnd = False
          Navigation.TabAdvanceDirection = adLeftRight
          ColumnSize.Location = clRegistry
          CellNode.Color = clSilver
          CellNode.NodeColor = clBlack
          CellNode.ShowTree = False
          MaxEditLength = 0
          MouseActions.DisjunctCellSelect = True
          MouseActions.RangeSelectAndEdit = True
          IntelliPan = ipVertical
          URLColor = clBlue
          URLShow = False
          URLFull = False
          URLEdit = False
          ScrollType = ssNormal
          ScrollColor = clNone
          ScrollWidth = 17
          ScrollSynch = False
          ScrollProportional = False
          ScrollHints = shNone
          OemConvert = False
          FixedFooters = 0
          FixedRightCols = 0
          FixedColWidth = 44
          FixedRowHeight = 21
          FixedFont.Charset = ANSI_CHARSET
          FixedFont.Color = clWindowText
          FixedFont.Height = -13
          FixedFont.Name = 'Arial'
          FixedFont.Style = []
          FixedAsButtons = False
          FloatFormat = '%.2f'
          IntegralHeight = False
          WordWrap = False
          ColumnHeaders.Strings = (
            'Depth\n(cm)'
            'SWCon'
            'NO3\n(ppm)'
            'NH4\n(ppm)'
            'FBiom'
            'FInert'
            'OC'
            'EC'
            'PH'
            'CL'
            'CEC'
            'Ca'
            'Mg'
            'Na'
            'K'
            'Exchangable\nSodium'
            'Particle Size\nSand'
            'Particle Size\nSilt'
            'Particle Size\nClay')
          Lookup = False
          LookupCaseSensitive = False
          LookupHistory = False
          BackGround.Top = 0
          BackGround.Left = 0
          BackGround.Display = bdTile
          BackGround.Cells = bcNormal
          Filter = <>
          ColWidths = (
            44
            64
            45
            47
            46
            51
            50
            35
            40
            40
            40
            51
            40
            44
            35
            35
            89
            87
            84)
          RowHeights = (
            41
            21
            21
            21
            21
            21
            21
            21
            21
            21
            21
            21
            21
            21
            21)
        end
      end
      object APSIMSheet: TTabSheet
        Caption = 'APSIM'
        ImageIndex = 2
        object ApsimGrid: TAdvStringGrid
          Left = 0
          Top = 0
          Width = 576
          Height = 501
          Cursor = crDefault
          Align = alClient
          ColCount = 2
          DefaultRowHeight = 21
          DefaultDrawing = False
          FixedCols = 1
          RowCount = 18
          FixedRows = 0
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Arial'
          Font.Style = []
          GridLineWidth = 1
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goDrawFocusSelected]
          ParentFont = False
          TabOrder = 0
          OnKeyDown = GridKeyDown
          OnMouseUp = GridMouseUp
          OnSetEditText = GridSetEditText
          GridLineColor = clSilver
          ActiveCellShow = False
          ActiveCellFont.Charset = DEFAULT_CHARSET
          ActiveCellFont.Color = clWindowText
          ActiveCellFont.Height = -11
          ActiveCellFont.Name = 'MS Sans Serif'
          ActiveCellFont.Style = [fsBold]
          ActiveCellColor = clGray
          Bands.PrimaryColor = clInfoBk
          Bands.PrimaryLength = 1
          Bands.SecondaryColor = clWindow
          Bands.SecondaryLength = 1
          Bands.Print = False
          AutoNumAlign = False
          AutoSize = False
          VAlignment = vtaTop
          EnhTextSize = False
          EnhRowColMove = False
          SizeWithForm = False
          Multilinecells = False
          OnCanEditCell = GridCanEditCell
          OnIsFixedCell = ApsimGridIsFixedCell
          DragDropSettings.OleAcceptFiles = True
          DragDropSettings.OleAcceptText = True
          SortSettings.AutoColumnMerge = False
          SortSettings.Column = 0
          SortSettings.Show = False
          SortSettings.IndexShow = False
          SortSettings.IndexColor = clYellow
          SortSettings.Full = True
          SortSettings.SingleColumn = False
          SortSettings.IgnoreBlanks = False
          SortSettings.BlankPos = blFirst
          SortSettings.AutoFormat = True
          SortSettings.Direction = sdAscending
          SortSettings.FixedCols = False
          SortSettings.NormalCellsOnly = False
          SortSettings.Row = 0
          FloatingFooter.Color = clBtnFace
          FloatingFooter.Column = 0
          FloatingFooter.FooterStyle = fsFixedLastRow
          FloatingFooter.Visible = False
          ControlLook.Color = clBlack
          ControlLook.CheckSize = 15
          ControlLook.RadioSize = 10
          ControlLook.ControlStyle = csClassic
          ControlLook.FlatButton = False
          EnableBlink = False
          EnableHTML = True
          EnableWheel = True
          Flat = False
          HintColor = clInfoBk
          SelectionColor = clHighlight
          SelectionTextColor = clHighlightText
          SelectionRectangle = False
          SelectionResizer = False
          SelectionRTFKeep = False
          HintShowCells = False
          HintShowLargeText = False
          HintShowSizing = False
          PrintSettings.FooterSize = 0
          PrintSettings.HeaderSize = 0
          PrintSettings.Time = ppNone
          PrintSettings.Date = ppNone
          PrintSettings.DateFormat = 'dd/mm/yyyy'
          PrintSettings.PageNr = ppNone
          PrintSettings.Title = ppNone
          PrintSettings.Font.Charset = DEFAULT_CHARSET
          PrintSettings.Font.Color = clWindowText
          PrintSettings.Font.Height = -11
          PrintSettings.Font.Name = 'MS Sans Serif'
          PrintSettings.Font.Style = []
          PrintSettings.HeaderFont.Charset = DEFAULT_CHARSET
          PrintSettings.HeaderFont.Color = clWindowText
          PrintSettings.HeaderFont.Height = -11
          PrintSettings.HeaderFont.Name = 'MS Sans Serif'
          PrintSettings.HeaderFont.Style = []
          PrintSettings.FooterFont.Charset = DEFAULT_CHARSET
          PrintSettings.FooterFont.Color = clWindowText
          PrintSettings.FooterFont.Height = -11
          PrintSettings.FooterFont.Name = 'MS Sans Serif'
          PrintSettings.FooterFont.Style = []
          PrintSettings.Borders = pbNoborder
          PrintSettings.BorderStyle = psSolid
          PrintSettings.Centered = False
          PrintSettings.RepeatFixedRows = False
          PrintSettings.RepeatFixedCols = False
          PrintSettings.LeftSize = 0
          PrintSettings.RightSize = 0
          PrintSettings.ColumnSpacing = 0
          PrintSettings.RowSpacing = 0
          PrintSettings.TitleSpacing = 0
          PrintSettings.Orientation = poPortrait
          PrintSettings.PageNumberOffset = 0
          PrintSettings.MaxPagesOffset = 0
          PrintSettings.FixedWidth = 0
          PrintSettings.FixedHeight = 0
          PrintSettings.UseFixedHeight = False
          PrintSettings.UseFixedWidth = False
          PrintSettings.FitToPage = fpNever
          PrintSettings.PageNumSep = '/'
          PrintSettings.NoAutoSize = False
          PrintSettings.NoAutoSizeRow = False
          PrintSettings.PrintGraphics = False
          HTMLSettings.Width = 100
          HTMLSettings.XHTML = False
          Navigation.AdvanceOnEnter = True
          Navigation.AdvanceDirection = adTopBottom
          Navigation.AllowClipboardShortCuts = True
          Navigation.AllowClipboardAlways = True
          Navigation.AdvanceAuto = True
          Navigation.InsertPosition = pInsertBefore
          Navigation.HomeEndKey = heFirstLastColumn
          Navigation.TabToNextAtEnd = False
          Navigation.TabAdvanceDirection = adLeftRight
          ColumnSize.Location = clRegistry
          CellNode.Color = clSilver
          CellNode.NodeColor = clBlack
          CellNode.ShowTree = True
          MaxEditLength = 0
          MouseActions.DisjunctCellSelect = True
          MouseActions.RangeSelectAndEdit = True
          IntelliPan = ipVertical
          URLColor = clBlue
          URLShow = False
          URLFull = False
          URLEdit = False
          ScrollType = ssNormal
          ScrollColor = clNone
          ScrollWidth = 17
          ScrollSynch = False
          ScrollProportional = False
          ScrollHints = shNone
          OemConvert = False
          FixedFooters = 0
          FixedRightCols = 0
          FixedColWidth = 139
          FixedRowHeight = 21
          FixedFont.Charset = ANSI_CHARSET
          FixedFont.Color = clWindowText
          FixedFont.Height = -13
          FixedFont.Name = 'Arial'
          FixedFont.Style = []
          FixedAsButtons = False
          FloatFormat = '%.2f'
          IntegralHeight = False
          WordWrap = False
          RowHeaders.Strings = (
            'Evaporation'
            'u:'
            'Cona:'
            'Salb:'
            'Unsaturated flow'
            'Diffus_const:'
            'Diffus_slope:'
            'Runoff'
            'CN2_Bare:'
            'CN_Red:'
            'CN_Cov:'
            'Organic matter'
            'Root_cn:'
            'Root_wt:'
            'Soil_cn:'
            'Erosion'
            'enr_a_coeff:'
            'enr_b_coeff:')
          Lookup = False
          LookupCaseSensitive = False
          LookupHistory = False
          BackGround.Top = 0
          BackGround.Left = 0
          BackGround.Display = bdTile
          BackGround.Cells = bcNormal
          Filter = <>
          ColWidths = (
            139
            74)
          RowHeights = (
            21
            21
            21
            21
            21
            21
            21
            21
            21
            21
            21
            21
            21
            21
            21
            21
            21
            21)
        end
      end
    end
  end
end
