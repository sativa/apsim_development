inherited ProbabilityForm: TProbabilityForm
  Caption = 'ProbabilityForm'
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
      object Label3: TLabel
        Left = 13
        Top = 26
        Width = 67
        Height = 13
        Caption = 'Field name:'
      end
      object FieldNameCombo: TComboBox
        Left = 13
        Top = 46
        Width = 141
        Height = 21
        BevelKind = bkSoft
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        TabOrder = 0
        OnChange = FieldNameComboChange
      end
      object ExceedenceCheckBox: TCheckBox
        Left = 14
        Top = 78
        Width = 188
        Height = 17
        Caption = 'Probability of exceedance?'
        TabOrder = 1
        OnClick = ExceedenceCheckBoxClick
      end
    end
  end
end
