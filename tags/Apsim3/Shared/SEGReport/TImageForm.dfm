object ImageForm: TImageForm
  Left = 250
  Top = 114
  Width = 258
  Height = 423
  Caption = 'ImageForm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 250
    Height = 389
    ActivePage = Properties
    Align = alClient
    TabIndex = 0
    TabOrder = 0
    object Properties: TTabSheet
      Caption = 'Properties'
      DesignSize = (
        242
        361)
      object Label1: TLabel
        Left = 8
        Top = 8
        Width = 48
        Height = 13
        Caption = 'Image file:'
      end
      object StretchCheckBox: TCheckBox
        Left = 16
        Top = 112
        Width = 97
        Height = 17
        Caption = 'Stretch'
        TabOrder = 0
        OnClick = StretchCheckBoxClick
      end
      object CentreCheckBox: TCheckBox
        Left = 16
        Top = 88
        Width = 97
        Height = 17
        Caption = 'Centre'
        TabOrder = 1
        OnClick = CentreCheckBoxClick
      end
      object AutoSizeCheckBox: TCheckBox
        Left = 16
        Top = 64
        Width = 97
        Height = 17
        Caption = 'Autosize'
        TabOrder = 2
        OnClick = AutoSizeCheckBoxClick
      end
      object ImageFileEdit: TAdvEditBtn
        Left = 8
        Top = 24
        Width = 231
        Height = 21
        AutoFocus = False
        EditAlign = eaLeft
        EditType = etString
        ErrorColor = clRed
        ErrorFontColor = clWhite
        ExcelStyleDecimalSeparator = False
        Flat = False
        FlatLineColor = clBlack
        FlatParentColor = True
        FocusAlign = eaDefault
        FocusBorder = False
        FocusColor = clWindow
        FocusFontColor = clWindowText
        FocusLabel = False
        FocusWidthInc = 0
        ModifiedColor = clHighlight
        DisabledColor = clSilver
        URLColor = clBlue
        ReturnIsTab = False
        LengthLimit = 0
        TabOnFullLength = False
        Precision = 0
        LabelPosition = lpLeftTop
        LabelMargin = 4
        LabelTransparent = False
        LabelAlwaysEnabled = False
        LabelFont.Charset = DEFAULT_CHARSET
        LabelFont.Color = clWindowText
        LabelFont.Height = -11
        LabelFont.Name = 'MS Sans Serif'
        LabelFont.Style = []
        Persistence.Enable = False
        Persistence.Location = plInifile
        Anchors = [akLeft, akTop, akRight]
        Color = clWindow
        Enabled = True
        HintShowLargeText = False
        OleDropTarget = False
        OleDropSource = False
        Signed = False
        TabOrder = 3
        Transparent = False
        Visible = True
        ButtonWidth = 18
        Etched = False
        Glyph.Data = {
          86030000424D8603000000000000B6000000280000000F0000000C0000000100
          200000000000D002000000000000000000001000000000000000000000000000
          80000080000000808000800000008000800080800000C0C0C000808080000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF0093714AFFB4AE
          A3FFC5D0D6FFC5D0D6FFC5D0D6FFC5D0D6FFC5D0D6FFC5D0D6FFC5D0D6FFC5D0
          D6FFC5D0D6FFC4D0D5FFC3CFD5FFB6B7B1FF906E48FFCAC7BFFFDCDDBF000000
          000000000000000000000000000000000000000000000000000000000000DCDD
          BF00DCDDBF00DCDDBF00CDD7DBFFBDBDB6FFEBF0F0FF00000000000000008080
          800080808000808080008080800080808000808080008080800000000000DCDD
          BF00DCDDBF00E1E8EAFFD9E2E4FFECF1F1FF00000000FFFFFF00000000008080
          800080808000808080008080800080808000808080008080800000000000DCDD
          BF00E3EAEBFFDBE2E5FFEDF2F2FF00000000FFFFFF00FFFFFF00000000008080
          800080808000808080008080800080808000808080008080800000000000E4EB
          ECFFDCE3E6FFEEF2F2FF00000000FFFFFF00FFFFFF00FFFFFF00000000000000
          0000000000000000000000000000000000000000000000000000E5EBECFFDDE3
          E6FFF3F6F6FF00000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF0000000000DCDDBF00DCDDBF00DCDDBF00EAF0F1FFE1E8EBFFF4F6
          F6FF00000000FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
          000000000000DCDDBF00DCDDBF00DCDDBF00EBF0F1FFE2E8EBFFF7F8F8FFDCDD
          BF00000000000000000000000000DCDDBF00DCDDBF00DCDDBF00DCDDBF00DCDD
          BF00000000000000000000000000EEF1F3FFE6EAEDFFFAFAFAFFDCDDBF00DCDD
          BF00DCDDBF00DCDDBF00DCDDBF00DCDDBF00DCDDBF00DCDDBF00DCDDBF00DCDD
          BF000000000000000000F2F4F5FFEAEEF0FFDCCEBFFFDCDDBF00DCDDBF00DCDD
          BF00DCDDBF00DCDDBF0000000000DCDDBF00DCDDBF00DCDDBF0000000000DCDD
          BF0000000000FDFEFEFFDBCEBFFFA27D55FFDCDDBF00DCDDBF00DCDDBF00DCDD
          BF00DCDDBF00DCDDBF00000000000000000000000000DCDDBF00DCDDBF00DCDD
          BF00E8DFD5FFA27D55FF}
        OnClickBtn = AdvEditBtn1Click
      end
    end
    object TabSheet1: TTabSheet
      Caption = 'Advanced'
      ImageIndex = 1
      DesignSize = (
        242
        361)
      object Label2: TLabel
        Left = 8
        Top = 8
        Width = 31
        Height = 13
        Caption = 'Name:'
      end
      object NameEdit: TEdit
        Left = 40
        Top = 8
        Width = 193
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        OnExit = NameEditExit
      end
    end
  end
  object OpenPictureDialog: TOpenPictureDialog
    Left = 120
    Top = 120
  end
end
