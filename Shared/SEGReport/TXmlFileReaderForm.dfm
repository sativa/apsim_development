inherited XmlFileReaderForm: TXmlFileReaderForm
  Left = 1765
  Top = 321
  Width = 289
  Caption = 'XmlFileReaderForm'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited AdvPanelGroup1: TAdvPanelGroup
    Width = 281
    FullHeight = 0
    inherited AdvancedPanel: TAdvPanel
      Width = 265
      FullHeight = 293
      inherited NameEdit: TEdit
        Width = 208
      end
      inherited SourceCombo: TComboBox
        Width = 208
      end
      inherited SortFieldsEdit: TEdit
        Width = 208
      end
      inherited GroupByEdit: TEdit
        Width = 209
      end
    end
    inherited PropertyPanel: TAdvPanel
      Width = 265
      FullHeight = 325
      object Label3: TLabel
        Left = 8
        Top = 24
        Width = 62
        Height = 13
        Caption = 'Filenames:'
      end
      object Label1: TLabel
        Left = 16
        Top = 74
        Width = 42
        Height = 13
        Cursor = crHandPoint
        Caption = 'Browse'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'Verdana'
        Font.Style = [fsUnderline]
        ParentFont = False
        OnClick = Label1Click
      end
      object FileNameEdit: TEdit
        Left = 16
        Top = 48
        Width = 225
        Height = 21
        TabOrder = 0
      end
    end
  end
  object OpenDialog: TOpenDialog
    DefaultExt = 'xml'
    Filter = 'Xml files (*.xml)|*.xml'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofEnableSizing]
    Left = 116
    Top = 136
  end
end
