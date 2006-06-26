inherited TextForm: TTextForm
  Width = 278
  Height = 527
  Caption = 'TextForm'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited AdvPanelGroup1: TAdvPanelGroup
    Width = 270
    Height = 494
    FullHeight = 18
    inherited AdvancedPanel: TAdvPanel
      Width = 254
      FullHeight = 293
      inherited NameEdit: TEdit
        Width = 183
      end
      inherited SourceCombo: TComboBox
        Width = 183
      end
      inherited SortFieldsEdit: TEdit
        Width = 183
      end
      inherited GroupByEdit: TEdit
        Width = 184
      end
    end
    inherited PropertyPanel: TAdvPanel
      Width = 254
      FullHeight = 297
      object Label6: TLabel
        Left = 7
        Top = 98
        Width = 30
        Height = 13
        Caption = 'Text:'
      end
      object Label7: TLabel
        Left = 7
        Top = 20
        Width = 62
        Height = 13
        Caption = 'Alignment:'
      end
      object Label5: TLabel
        Left = 7
        Top = 267
        Width = 191
        Height = 65
        Anchors = [akLeft, akBottom]
        Caption = 
          'E.g macros:    $precision(component.property,                   ' +
          'decplaces [,recno])  $property(component.property               ' +
          '    [,recno])'
        WordWrap = True
      end
      object FontLabel: TLabel
        Left = 7
        Top = 78
        Width = 49
        Height = 13
        Cursor = crHandPoint
        Caption = 'Edit Font'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'Verdana'
        Font.Style = [fsUnderline]
        ParentFont = False
        OnClick = FontLabelClick
      end
      object TextEdit: TRichEdit
        Left = 7
        Top = 111
        Width = 189
        Height = 156
        Anchors = [akLeft, akTop, akRight, akBottom]
        BevelInner = bvNone
        BevelOuter = bvRaised
        BevelKind = bkFlat
        BorderStyle = bsNone
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
        OnExit = TextEditExit
      end
      object AlignmentCombo: TComboBox
        Left = 7
        Top = 33
        Width = 143
        Height = 21
        BevelKind = bkSoft
        ItemHeight = 13
        TabOrder = 2
        OnChange = AlignmentComboChange
        Items.Strings = (
          'Left'
          'Centre'
          'Right')
      end
      object AutosizeCheckBox: TCheckBox
        Left = 7
        Top = 59
        Width = 96
        Height = 17
        Alignment = taLeftJustify
        Caption = 'Autosize?'
        Ctl3D = True
        ParentCtl3D = False
        TabOrder = 1
        OnClick = ToolbarCheckBoxClick
      end
    end
  end
  object FontDialog: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    MinFontSize = 0
    MaxFontSize = 0
    Left = 144
    Top = 248
  end
end
