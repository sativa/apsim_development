inherited TextForm: TTextForm
  Width = 278
  Height = 527
  Caption = 'TextForm'
  OldCreateOrder = True
  PixelsPerInch = 120
  TextHeight = 16
  inherited AdvPanelGroup1: TAdvPanelGroup
    Width = 270
    Height = 494
    FullHeight = 18
    inherited AdvancedPanel: TAdvPanel
      Width = 254
      FullHeight = 293
      inherited NameEdit: TEdit
        Width = 166
      end
      inherited SourceCombo: TComboBox
        Width = 166
      end
      inherited SortFieldsEdit: TEdit
        Width = 166
      end
      inherited GroupByEdit: TEdit
        Width = 167
      end
    end
    inherited PropertyPanel: TAdvPanel
      Width = 254
      FullHeight = 297
      object Label6: TLabel
        Left = 8
        Top = 120
        Width = 36
        Height = 16
        Caption = 'Text:'
      end
      object Label7: TLabel
        Left = 8
        Top = 24
        Width = 70
        Height = 16
        Caption = 'Alignment:'
      end
      object Label5: TLabel
        Left = 8
        Top = 328
        Width = 231
        Height = 64
        Anchors = [akLeft, akBottom]
        Caption = 
          'E.g macros:    $precision(component.property,                   ' +
          'decplaces)  $property(component.property)'
        WordWrap = True
      end
      object FontLabel: TLabel
        Left = 8
        Top = 96
        Width = 60
        Height = 16
        Cursor = crHandPoint
        Caption = 'Edit Font'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -13
        Font.Name = 'Verdana'
        Font.Style = [fsUnderline]
        ParentFont = False
        OnClick = FontLabelClick
      end
      object TextEdit: TRichEdit
        Left = 8
        Top = 136
        Width = 233
        Height = 193
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
        Left = 8
        Top = 40
        Width = 176
        Height = 24
        BevelKind = bkSoft
        ItemHeight = 16
        TabOrder = 2
        OnChange = AlignmentComboChange
        Items.Strings = (
          'Left'
          'Centre'
          'Right')
      end
      object AutosizeCheckBox: TCheckBox
        Left = 8
        Top = 72
        Width = 119
        Height = 21
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
