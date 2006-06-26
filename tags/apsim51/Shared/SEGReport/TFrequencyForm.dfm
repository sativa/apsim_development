inherited FrequencyForm: TFrequencyForm
  Left = 142
  Top = 65
  Caption = 'FrequencyForm'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited AdvPanelGroup1: TAdvPanelGroup
    FullHeight = 18
    inherited AdvancedPanel: TAdvPanel
      FullHeight = 310
      inherited NameEdit: TEdit
        Width = 102
      end
      inherited SourceCombo: TComboBox
        Width = 102
      end
      inherited SortFieldsEdit: TEdit
        Width = 132
      end
      inherited GroupByEdit: TEdit
        Width = 132
      end
    end
    inherited PropertyPanel: TAdvPanel
      FullHeight = 38
      object Grid: TStringGrid
        Left = 8
        Top = 32
        Width = 273
        Height = 297
        ColCount = 2
        DefaultRowHeight = 20
        FixedCols = 0
        RowCount = 50
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing]
        TabOrder = 0
        OnExit = GridEditingDone
        ColWidths = (
          101
          146)
      end
    end
  end
end
