inherited ApsimFileReaderForm: TApsimFileReaderForm
  Width = 289
  Caption = 'ApsimFileReaderForm'
  OldCreateOrder = True
  PixelsPerInch = 120
  TextHeight = 16
  inherited AdvPanelGroup1: TAdvPanelGroup
    Width = 281
    FullHeight = 0
    inherited AdvancedPanel: TAdvPanel
      Width = 265
      FullHeight = 293
      inherited NameEdit: TEdit
        Width = 195
      end
      inherited SourceCombo: TComboBox
        Width = 195
      end
      inherited SortFieldsEdit: TEdit
        Width = 195
      end
      inherited GroupByEdit: TEdit
        Width = 196
      end
    end
    inherited PropertyPanel: TAdvPanel
      Width = 265
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
      object Label4: TLabel
        Left = 211
        Top = 189
        Width = 33
        Height = 16
        Cursor = crHandPoint
        Alignment = taRightJustify
        Anchors = [akTop, akRight]
        Caption = 'Clear'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -13
        Font.Name = 'Verdana'
        Font.Style = [fsUnderline]
        ParentFont = False
        OnClick = Label4Click
      end
      object FilesList: TListView
        Left = 8
        Top = 64
        Width = 239
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
