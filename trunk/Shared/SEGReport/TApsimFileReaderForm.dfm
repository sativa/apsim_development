inherited ApsimFileReaderForm: TApsimFileReaderForm
  Caption = 'ApsimFileReaderForm'
  OldCreateOrder = True
  PixelsPerInch = 120
  TextHeight = 16
  inherited AdvPanelGroup1: TAdvPanelGroup
    FullHeight = 0
    inherited AdvancedPanel: TAdvPanel
      FullHeight = 293
      inherited NameEdit: TEdit
        Width = 194
      end
      inherited SourceCombo: TComboBox
        Width = 194
      end
      inherited SortFieldsEdit: TEdit
        Width = 194
      end
      inherited GroupByEdit: TEdit
        Width = 195
      end
    end
    inherited PropertyPanel: TAdvPanel
      FullHeight = 325
      object Label3: TLabel
        Left = 10
        Top = 29
        Width = 70
        Height = 16
        Caption = 'Filenames:'
      end
      object Label1: TLabel
        Left = 10
        Top = 189
        Width = 47
        Height = 16
        Cursor = crHandPoint
        Caption = 'Browse'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -13
        Font.Name = 'Verdana'
        Font.Style = [fsUnderline]
        ParentFont = False
        OnClick = Label1Click
      end
      object FilesList: TListView
        Left = 8
        Top = 64
        Width = 238
        Height = 121
        Anchors = [akLeft, akTop, akRight]
        BevelInner = bvLowered
        BevelOuter = bvRaised
        BevelKind = bkSoft
        BorderStyle = bsNone
        Columns = <
          item
            AutoSize = True
          end>
        TabOrder = 0
        ViewStyle = vsList
      end
      object InterpretCheckBox: TCheckBox
        Left = 8
        Top = 229
        Width = 177
        Height = 21
        Alignment = taLeftJustify
        Caption = 'Whopper style titles?'
        Ctl3D = True
        ParentCtl3D = False
        TabOrder = 1
        OnClick = InterpretCheckBoxClick
      end
    end
  end
  object OpenDialog: TOpenDialog
    DefaultExt = 'out'
    Filter = 'Output files (*.out)|*.out|All files (*.*)|*.*'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofEnableSizing]
    Left = 116
    Top = 136
  end
end
